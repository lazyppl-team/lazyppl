# /// script
# requires-python = ">=3.10"
# dependencies = ["numpy", "plotly"]
# ///
"""Plot the points that GraphDemo.lhs writes to graph-sphere.csv on the unit sphere.

Each pair of points at angular distance less than THETA is joined by a geodesic arc.
The figure is written to graph-sphere.html, the file that the GraphDemo page embeds.

    uv run geometric_graph_plot.py THETA [--csv graph-sphere.csv] [--out graph-sphere]

THETA is the value that GraphDemo prints as "Theta:". With --png the figure is also
written as a PNG image, which needs kaleido and a Chrome or Chromium browser:

    uv run --with kaleido geometric_graph_plot.py THETA --png
"""
import argparse
from itertools import combinations

import numpy as np
import plotly.graph_objs as go


def sphere_arc(p1, p2, n=100):
    """n points on the great-circle arc between the unit vectors p1 and p2."""
    t = np.linspace(0, 1, num=n)
    x = np.interp(t, [0, 1], [p1[0], p2[0]])
    y = np.interp(t, [0, 1], [p1[1], p2[1]])
    z = np.interp(t, [0, 1], [p1[2], p2[2]])

    line_points = np.array([x, y, z]).T
    norm = np.linalg.norm(line_points, axis=1)
    return line_points / norm[:, np.newaxis]


def angular_distance(p1, p2):
    return np.arccos(np.dot(p1, p2))


def plot(points, theta, out="graph-sphere", png=False):
    """Write out.html (and out.png if png is set) for the unit vectors in points (an n x 3 array)."""
    geo_curves = [sphere_arc(n1, n2) for n1, n2 in combinations(points, 2)
                  if not np.array_equal(n1, n2) and angular_distance(n1, n2) < theta]

    fig = go.Figure(data=[go.Scatter3d(x=points[:, 0], y=points[:, 1], z=points[:, 2],
            mode='markers',
            marker=dict(size=8, color='black', opacity=1, symbol="circle"))])

    # Add the geodesic curves to the figure
    for curve in geo_curves:
        fig.add_trace(go.Scatter3d(x=curve[:, 0], y=curve[:, 1], z=curve[:, 2], mode='lines', line=dict(width=3, color='black')))

    # Define the sphere
    u = np.linspace(0, 2 * np.pi, 100)
    v = np.linspace(0, np.pi, 100)
    x = 1 * np.outer(np.cos(u), np.sin(v))
    y = 1 * np.outer(np.sin(u), np.sin(v))
    z = 1 * np.outer(np.ones(np.size(u)), np.cos(v))

    p = 0.96
    # Create the sphere
    opacity = 0.6

    fig.add_trace(go.Surface(x=p*x, y=p*y, z=p*z, colorscale='viridis', showscale=False, opacity=opacity))

    # Set the camera and the axis
    fig.update_layout(
        scene=dict(
            camera=dict(
                up=dict(x=0, y=0, z=1),
                center=dict(x=0, y=0, z=0),
                eye=dict(x=1.25, y=1.25, z=1.25),
            ),
            yaxis=dict(showbackground=False, visible=False, showgrid=False, showticklabels=False),
            xaxis=dict(showbackground=False, visible=False, showgrid=False, showticklabels=False),
            zaxis=dict(showbackground=False, visible=False, showgrid=False, showticklabels=False),
            aspectmode='manual',
        ),
        margin=dict(r=1, l=1, b=1, t=1),
        showlegend=False
        )

    fig.write_html(f"{out}.html", include_plotlyjs="cdn", full_html=False)
    if png:
        fig.write_image(f"{out}.png")


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("theta", type=float, help="arcs join the points closer than this angle (radians)")
    parser.add_argument("--csv", default="graph-sphere.csv", help="the points, one x,y,z row each")
    parser.add_argument("--out", default="graph-sphere", help="output file name without extension")
    parser.add_argument("--png", action="store_true", help="also write OUT.png")
    args = parser.parse_args()
    plot(np.loadtxt(args.csv, delimiter=','), args.theta, args.out, args.png)


if __name__ == "__main__":
    main()
