#[expect(clippy::ref_option)]
pub(crate) fn format_error(e: &Option<String>) -> String {
    let Some(e) = e else {
        return String::new();
    };
    String::from(". ") + e
}
