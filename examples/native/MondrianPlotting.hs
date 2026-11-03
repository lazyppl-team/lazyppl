{-# LANGUAGE ExtendedDefaultRules, ScopedTypeVariables, BangPatterns #-}
module MondrianPlotting where
import LazyPPL.Distributions.Mondrian
import Data.Map (Map,keys,(!))
import Data.Colour.RGBSpace.HSV
import Data.Colour.SRGB
    ( RGB(channelBlue, channelGreen, channelRed) )

import Graphics.Matplotlib
    ( subplots,
      text,
      xlim,
      ylim,
      (#),
      (##),
      (%),
      (@@),
      lit,
      mp,
      o2,
      readData,
      Matplotlib )

import qualified Text.Printf

plotMondrian2D :: Mondrian Double -> Matplotlib
plotMondrian2D mondrian = case mondrian of
  Block p abs@(~[(a1, b1), (a2, b2)]) ->
    mp # "ax.add_patch(plot.Polygon(" # polygonFromBounds abs ## "))"
        @@ [o2 "facecolor" "0.9", o2 "edgecolor" "0.5"]
    % text ((a1 + b1)/2) ((a2 + b2)/2) ("$" ++ Text.Printf.printf "%.2f" p ++ "$")
      @@ [o2 "horizontalalignment" "center", o2 "fontsize" 20]
  Partition _ _ ~[(a1, b1), (a2, b2)] left right ->
    subplots % xlim a1 b1 % ylim a2 b2
      % helper left % helper right
  where
    polygonFromBounds ~[(a1, b1), (a2, b2)] = [(a1, a2), (b1, a2), (b1, b2), (a1, b2)]
    helper (Block p abs@(~[(a1, b1), (a2, b2)])) =
      let c = hsv (p * 360) 0.4 1
          rgba alpha = (channelRed c, channelGreen c, channelBlue c, alpha)
          pythonRgba alpha = lit $ show $ rgba alpha
      in mp # "ax.add_patch(plot.Polygon(" # polygonFromBounds abs ## "))"
            @@ [o2 "facecolor" (pythonRgba 0.9),
            o2 "edgecolor" "0.3",
            o2 "linewidth" 1.5]
          % text ((a1 + b1)/2) ((a2 + b2)/2) ("$" ++ Text.Printf.printf "%.2f" p ++ "$")
            @@ [o2 "horizontalalignment" "center", o2 "fontsize" 10]
    helper (Partition _ _ _ left right) = helper left % helper right

{- | Display dots corresponding to the pairs (ξ_i , η_j) of a finite relation in the plot -}
plotMapRelation :: Matplotlib -> Map (Double, Double) Bool -> Matplotlib
plotMapRelation mp rel =
  readData (xs, ys, m, c)
  % mp # newDefScatter
  # "mscatter(data[0], data[1], ax=ax, s=60, linewidths=2, m=data[2], c=data[3], alpha=0.8, zorder=2)"
  where
  ks = keys rel
  (xs, ys) = unzip ks
  -- green = (200/255, 247/255, 197/255, 1)
  -- red = (254/255, 121/255, 104/255, 1)
  (m, c) = unzip $ map (\(x, y) -> if rel ! (x, y) then ("+", "g") else ("x", "r")) ks
  -- custom definition of `scatter` to support a list of markers
  -- credit: https://github.com/matplotlib/matplotlib/issues/11155#issuecomment-385939618
  newDefScatter = unlines
    [ ""
    , "def mscatter(x, y, ax=None, m=None, **kw):"
    , "  import matplotlib.markers as mmarkers"
    , "  if not ax: ax=plt.gca()"
    , "  sc = ax.scatter(x,y,**kw)"
    , "  if (m is not None) and (len(m)==len(x)):"
    , "    paths = []"
    , "    for marker in m:"
    , "      if isinstance(marker, mmarkers.MarkerStyle):"
    , "        marker_obj = marker"
    , "      else:"
    , "        marker_obj = mmarkers.MarkerStyle(marker)"
    , "      path = marker_obj.get_path().transformed(marker_obj.get_transform())"
    , "      paths.append(path)"
    , "    sc.set_paths(paths)"
    , "  return sc"
    ]

