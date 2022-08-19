#[cfg(feature = "glob")]
use regex_intersect::glob;
use regex_intersect::non_empty;
#[cfg(feature = "tracing")]
use tracing_subscriber::{filter, EnvFilter, Registry};
#[cfg(feature = "tracing")]
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};
#[cfg(feature = "tracing")]
use tracing_tree::HierarchicalLayer;

fn main() {
    #[cfg(feature = "tracing")]
    Registry::default()
        .with(HierarchicalLayer::new(2))
        .with(
            EnvFilter::builder()
                .with_default_directive(filter::LevelFilter::OFF.into())
                .with_env_var("LOG")
                .from_env_lossy(),
        )
        .init();

    let res = glob::non_empty("foo/*/baz", "foo/*/qux");
    println!("{:?}", res);
    // re_intersect("f.*", "fabcd");
    // re_intersect("fd.*", "fabcd");
    // re_intersect("fabcd", "f.*");
    // re_intersect("fabcd", "fd.*");
    // re_intersect("f.*abcd", "fd.*");
    // re_intersect("f.*abcd", "fd.*z");
    // re_intersect("f[a-n]*abcd", "fd.*z");
}
