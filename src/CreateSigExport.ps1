Remove-Item ".\DikesOvertopping_*.zip"
git archive --output ".\DikesOvertopping_source_$(get-date -Format yyyyMMdd).zip" --prefix="DikesOvertopping_source/" HEAD:src .