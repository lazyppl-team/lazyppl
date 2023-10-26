import plotly.graph_objs as go
import networkx as nx
import numpy as np
from numpy import loadtxt
from tqdm import tqdm 

def plot(fname, θ=np.pi/3):
    
    # project the points on the sphere
    points = loadtxt('graph-sphere.csv',delimiter=',')
        
    def sphere_arc(p1, p2, n=100):
        t = np.linspace(0, 1, num=n)
        x = np.interp(t, [0, 1], [p1[0], p2[0]])
        y = np.interp(t, [0, 1], [p1[1], p2[1]])
        z = np.interp(t, [0, 1], [p1[2], p2[2]])

        line_points = np.array([x, y, z]).T
        norm = np.linalg.norm(line_points, axis=1)
        return line_points / norm[:, np.newaxis]

    def dist(p1, p2):
        return np.arccos(np.dot(p1, p2))

    geo_curves = [sphere_arc(n1,n2) for n1 in points for n2 in points \
        if dist(n1,n2) < θ]
#    geo_curves = [sphere_arc(points[n1], points[n2]) for n1, n2 in G.edges() \
#        if dist(points[n1], points[n2]) < θ]

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

    fig.show()
    fig.write_image(f"{fname}.png")
    fig.write_html(f"{fname}.html",include_plotlyjs="cdn",full_html=False)

import sys
theta=float(sys.argv[1])
plot(f"graph-sphere", θ=theta)
