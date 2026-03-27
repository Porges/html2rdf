#![cfg(not(target_os = "windows"))]

use std::ffi::{CStr, CString};

#[allow(non_upper_case_globals)]
#[allow(non_camel_case_types)]
#[allow(non_snake_case)]
#[allow(unused)]
mod bindings {
    include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
}

unsafe extern "C" fn triple_handler<F: for<'a> FnMut(oxrdf::TripleRef<'a>)>(
    triple: *mut bindings::rdftriple,
    ctx: *mut std::ffi::c_void,
) {
    fn c_str(ptr: &*mut i8) -> Option<&str> {
        if ptr.is_null() {
            None
        } else {
            unsafe { CStr::from_ptr(*ptr).to_str().ok() }
        }
    }

    debug_assert!(!triple.is_null());
    debug_assert!(!ctx.is_null());
    unsafe {
        let callback = ctx as *mut CallbackContext<F>;
        if let (Some(subj), Some(pred), Some(obj)) = (
            c_str(&(*triple).subject),
            c_str(&(*triple).predicate),
            c_str(&(*triple).object),
        ) {
            // figuring out what is what is based upon
            // https://github.com/rdfa/librdfa/blob/10e530433c7a93157a4d24a31e0bc58dc333923a/c/triple.c#L81
            let subj_rdf: oxrdf::NamedOrBlankNodeRef = if subj.starts_with("_:") {
                oxrdf::BlankNodeRef::new_unchecked(&subj[2..]).into()
            } else {
                oxrdf::NamedNodeRef::new_unchecked(subj).into()
            };

            let pred_rdf = oxrdf::NamedNodeRef::new_unchecked(pred);
            let obj_rdf: oxrdf::TermRef = match (*triple).object_type {
                bindings::rdfresource_t_RDF_TYPE_IRI => {
                    if obj.starts_with("_:") {
                        oxrdf::BlankNodeRef::new_unchecked(&obj[2..]).into()
                    } else {
                        oxrdf::NamedNodeRef::new_unchecked(obj).into()
                    }
                }
                bindings::rdfresource_t_RDF_TYPE_PLAIN_LITERAL => {
                    if let Some(lang) = c_str(&(*triple).language) {
                        oxrdf::LiteralRef::new_language_tagged_literal_unchecked(obj, lang).into()
                    } else {
                        oxrdf::LiteralRef::new_simple_literal(obj).into()
                    }
                }
                bindings::rdfresource_t_RDF_TYPE_TYPED_LITERAL => {
                    if let Some(obj_type) = c_str(&(*triple).datatype) {
                        let obj_type_rdf = oxrdf::NamedNodeRef::new_unchecked(obj_type);
                        oxrdf::LiteralRef::new_typed_literal(obj, obj_type_rdf).into()
                    } else {
                        oxrdf::LiteralRef::new_simple_literal(obj).into()
                    }
                }
                _ => {
                    eprintln!("Unknown object type: {}", (*triple).object_type);
                    bindings::rdfa_free_triple(triple);
                    return;
                }
            };

            ((*callback).callback)(oxrdf::TripleRef::new(subj_rdf, pred_rdf, obj_rdf));
        }
        bindings::rdfa_free_triple(triple);
    }
}

unsafe extern "C" fn fill_buffer<F>(
    buf: *mut i8,
    size: usize,
    ctx: *mut std::ffi::c_void,
) -> usize {
    debug_assert!(!ctx.is_null());
    debug_assert!(!buf.is_null());
    unsafe {
        let callback = ctx as *mut CallbackContext<F>;
        let to_copy = std::cmp::min(size, (*callback).input_len - (*callback).bytes_read);
        std::ptr::copy_nonoverlapping((*callback).input.add((*callback).bytes_read), buf, to_copy);
        (*callback).bytes_read += to_copy;
        to_copy
    }
}

struct CallbackContext<F> {
    callback: F,
    input: *const i8,
    input_len: usize,
    bytes_read: usize,
}

pub fn run<F: for<'a> FnMut(oxrdf::TripleRef<'a>)>(base: &str, input: &str, callback: F) {
    static LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

    let base_c = CString::new(base).unwrap();
    let mut callback_ctx = CallbackContext {
        callback,
        input_len: input.len(),
        input: input.as_ptr() as *const i8,
        bytes_read: 0,
    };

    // librdfa uses libxml2 in a non-threadsafe way, so we must protect it
    let _guard = LOCK.lock().unwrap();
    unsafe {
        let ctx = bindings::rdfa_create_context(base_c.as_ptr());
        (*ctx).callback_data =
            (&mut callback_ctx) as *mut CallbackContext<F> as *mut std::ffi::c_void;
        bindings::rdfa_set_default_graph_triple_handler(ctx, Some(triple_handler::<F>));
        bindings::rdfa_set_buffer_filler(ctx, Some(fill_buffer::<F>));
        bindings::rdfa_parse(ctx);
        bindings::rdfa_free_context(ctx);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn render_triples(input: &str) -> Vec<String> {
        let base = "https://librdfa.test/";
        let mut output = Vec::new();
        let mut callback = |trip: oxrdf::TripleRef| {
            output.push(format!("{trip}"));
        };
        run(base, input, &mut callback);
        return output;
    }

    #[test]
    fn basic() {
        let input = r#"<html><head><title>Test</title></head>
            <body><p property="ex:prop">Value</p></body></html>"#;

        let output = render_triples(input);
        assert_eq!(output, vec![r#"<https://librdfa.test/> <ex:prop> "Value""#]);
    }

    #[test]
    fn language() {
        let input = r#"<html><head><title>Test</title></head>
            <body><p property="ex:prop" lang="fr">Valeur</p></body></html>"#;

        let output = render_triples(input);
        assert_eq!(
            output,
            vec![r#"<https://librdfa.test/> <ex:prop> "Valeur"@fr"#]
        );
    }

    #[test]
    fn datatype() {
        let input = r#"<html><head><title>Test</title></head>
            <body><p property="ex:prop" datatype="xds:int">123</p></body></html>"#;

        let output = render_triples(input);
        assert_eq!(
            output,
            vec![r#"<https://librdfa.test/> <ex:prop> "123"^^<xds:int>"#]
        );
    }

    #[test]
    fn vocab() {
        let input = r#"<html><head><title>Test</title></head>
            <body vocab="http://purl.org/dc/terms/"><p property="title">title</p></body></html>"#;

        let output = render_triples(input);
        assert_eq!(
            output,
            vec![
                r#"<https://librdfa.test/> <http://www.w3.org/ns/rdfa#usesVocabulary> <http://purl.org/dc/terms/>"#,
                r#"<https://librdfa.test/> <http://purl.org/dc/terms/title> "title""#,
            ]
        );
    }

    #[test]
    fn bnode() {
        let input = r#"<html><head><title>Test</title></head>
            <body><p typeof="title"><span property="prop">title</span></p></body></html>"#;

        let output = render_triples(input);
        assert_eq!(
            output,
            vec![
                r#"_:bnode0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://librdfa.test/title>"#,
                r#"_:bnode0 <https://librdfa.test/prop> "title""#,
            ]
        );
    }

    #[test]
    fn prefix() {
        let input = r#"<html><head><title>Test</title></head>
            <body prefix="foo: urn:example/"><p typeof="foo:title"><span property="prop">title</span></p></body></html>"#;

        let output = render_triples(input);
        assert_eq!(
            output,
            vec![
                r#"_:bnode0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <urn:example/title>"#,
                r#"_:bnode0 <https://librdfa.test/prop> "title""#,
            ]
        );
    }
}
