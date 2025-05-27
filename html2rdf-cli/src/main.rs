use std::process::ExitCode;

use clap::Parser;

#[derive(Parser)]
#[command(version, about)]
struct Args {
    #[arg(value_name = "URL")]
    target: url::Url,
}

fn main() -> Result<ExitCode, Box<dyn std::error::Error>> {
    let args = Args::parse();
    let client = reqwest::blocking::Client::new();
    let base = args.target.to_string();
    let base_iri = oxiri::Iri::parse(base.clone())?;
    let response = client.get(args.target).send()?.error_for_status()?;
    let content_type = response
        .headers()
        .get(reqwest::header::CONTENT_TYPE)
        .and_then(|v| v.to_str().ok());

    if content_type.is_some_and(|ct| !ct.starts_with("text/html")) {
        eprintln!("Error: content type is not text/html.");
        return Ok(ExitCode::FAILURE);
    }

    let content = response.text()?;
    let mut output_graph = oxrdf::Graph::new();
    let mut processor_graph = oxrdf::Graph::new();
    html2rdf::process(
        &content,
        base_iri.clone(),
        &mut output_graph,
        &mut processor_graph,
    )?;

    {
        // output any warnings/errors
        let serializer = oxttl::TurtleSerializer::new();
        let mut locked_err = std::io::stderr().lock();
        let mut writer = serializer.for_writer(&mut locked_err);
        for triple in processor_graph.iter() {
            writer.serialize_triple(triple)?;
        }

        writer.finish()?;
        drop(processor_graph);
    }

    {
        // use serializer with all known prefixes
        let serializer = html2rdf::initial_context_prefixes().mappings().try_fold(
            oxttl::TurtleSerializer::new().with_base_iri(base)?,
            |serializer, (prefix, value)| serializer.with_prefix(prefix, value),
        )?;

        let mut locked_out = std::io::stdout().lock();
        let mut writer = serializer.for_writer(&mut locked_out);
        for triple in output_graph.iter() {
            writer.serialize_triple(triple)?;
        }

        writer.finish()?;
        drop(output_graph);
    }

    Ok(ExitCode::SUCCESS)
}
