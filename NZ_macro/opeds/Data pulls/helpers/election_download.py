"""TLS-verified stdlib fallback for R/Windows Schannel handshake failures."""
import pathlib
import sys
import urllib.request

url, destination, expected_name = sys.argv[1:]
request = urllib.request.Request(url, headers={"User-Agent": "NZ economic evidence research"})
with urllib.request.urlopen(request, timeout=90) as response:
    if response.status != 200:
        raise RuntimeError(f"Unexpected HTTP status {response.status}")
    content_type = response.headers.get("Content-Type", "")
    data = response.read()
if len(data) < 20:
    raise RuntimeError("Empty source")
if not expected_name.endswith(".html") and "text/html" in content_type:
    raise RuntimeError("HTML response instead of statistical data")
pathlib.Path(destination).write_bytes(data)
print(content_type)
