"""Check the generated Plotly graph against the seeded program's CSV and theta."""
import argparse
import base64
import collections
import csv
import itertools
import json
import math
import struct
from pathlib import Path


def values(value):
    if isinstance(value, dict) and "bdata" in value:
        raw = base64.b64decode(value["bdata"])
        code = {"f8": "d", "f4": "f"}[value["dtype"]]
        return struct.unpack("<" + code * (len(raw) // struct.calcsize(code)), raw)
    return tuple(map(float, value))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("figure", type=Path)
    parser.add_argument("points", type=Path)
    parser.add_argument("theta", type=float)
    args = parser.parse_args()
    html = args.figure.read_text(encoding="utf-8")
    start = html.index("Plotly.newPlot(")
    data_start = html.index("[", html.index(",", start))
    traces, _ = json.JSONDecoder().raw_decode(html[data_start:])
    with args.points.open(newline="") as stream:
        points = [tuple(map(float, row)) for row in csv.reader(stream)]
    markers = [trace for trace in traces if trace.get("type") == "scatter3d" and trace.get("mode") == "markers"]
    assert len(markers) == 1, f"expected one marker trace, got {len(markers)}"
    actual_points = list(zip(*(values(markers[0][axis]) for axis in "xyz")))
    assert len(actual_points) == len(points) and all(math.dist(a, b) < 1e-12 for a, b in zip(actual_points, points)), "marker coordinates differ from the seeded program"

    expected = {pair for pair in itertools.combinations(range(len(points)), 2)
                if math.acos(max(-1.0, min(1.0, sum(a * b for a, b in zip(points[pair[0]], points[pair[1]]))))) < args.theta}
    edges = []
    zero_length = 0
    for trace in traces:
        if trace.get("type") != "scatter3d" or trace.get("mode") != "lines":
            continue
        coordinates = list(zip(*(values(trace[axis]) for axis in "xyz")))
        assert len(coordinates) >= 2, "edge trace has fewer than two points"
        start_point, end_point = coordinates[0], coordinates[-1]
        if math.dist(start_point, end_point) <= 1e-12:
            zero_length += 1
            continue
        ends = []
        for endpoint in (start_point, end_point):
            matches = [i for i, point in enumerate(points) if math.dist(endpoint, point) < 1e-12]
            assert len(matches) == 1, f"edge endpoint does not identify one seeded vertex: {endpoint}"
            ends.append(matches[0])
        edges.append(tuple(sorted(ends)))
    counts = collections.Counter(edges)
    duplicates = [pair for pair, count in counts.items() if count != 1]
    assert zero_length == 0 and not duplicates and set(edges) == expected, (
        f"expected {len(expected)} distinct pairs, got {len(edges)} nonzero traces; "
        f"zero-length={zero_length}, duplicates={duplicates}, "
        f"missing={sorted(expected - set(edges))}, extra={sorted(set(edges) - expected)}")
    print(f"PASS: {len(points)} seeded points, {len(edges)} distinct edges, no zero-length traces")


if __name__ == "__main__":
    main()
