import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)
import Html exposing (Html, text, div, span, tr, td, table, input, br, p, button)
import Html.App as Html
import Html.Attributes exposing (width, height, style, checked, type')
import Html.Events exposing (onCheck, onClick)
import AnimationFrame
import Complex exposing (arg, complex, abs, Complex, sub, add, i, mult, negation, euler, zero)
import Mouse
import Array exposing (Array)
import Unicode exposing (text')

type alias Vertex = { position : Vec3, color : Vec3 }
type alias Display = { show : Bool, leg : Bool }

type alias Model = { t : Float, angx : Float, angy : Float
                   , drag : Bool, oldpos : Mouse.Position
                   , psy : Array Complex
                   , evolve : Bool  
                   , evolutionSpeed : Int
                   , ndt : Int -- 1..5
                   , psyLeg : Bool
                   , dx : Display
                   , dx2 : Display
                   , h : Display
                   , momentum : Display
                   , speed : Display
                   , ampl : Display                 
                   }

initialState = { t=0, angx=0, angy=0, drag=False, oldpos={x=0, y=0}, psy = initQS particle
               , evolve = False
               , evolutionSpeed = 5
               , ndt = 2
               , psyLeg = False
               , dx = {show=False, leg=False}
               , dx2 = {show=False, leg=False}
               , h = {show=False, leg=False}
               , momentum = {show=False, leg=False}
               , speed = {show=False, leg=False}
               , ampl = {show=False, leg=False}
               }


-- wave functions
particle x = mult (complex (e ^ (-1.5*x*x)) 0) (euler (x*2.0))
part2 x = complex (2 * (sin x)* (e ^ (-0.5*x*x))) 0
part3 x = mult (complex (e ^ (-0.1*x*x)) 0) (Complex.add (euler (x*2)) (euler (x*3)))
ps x = [1, 1.3, 2.2] |> List.map (\w -> euler (x * w)) 
          |> List.foldl Complex.add (complex 0 0) |> mult (complex (e ^ (-0.1*x*x)) 0)
gauss x = complex (e ^ (-x*x)) 0 

-- constants
sizeQS = 1000 -- number of points in array
dx = 0.02     -- space between points
dx2 = dx*dx
i2dx2 = complex 0 (1/(2*dx2))  --  i / (2*dx*dx)
c2 = complex 2 0
overdx2 = complex (1/dx2) 0

-- UI
type Func = Dx | Dx2 | H | Momentum | Speed | Ampl

type Action = MouseMove Mouse.Position
            | MouseBttn Bool --true=down  
            | NewFrame Float --dt
            | Evolve Bool
            | PsyLeg Bool
            | Show Func Bool
            | ShowLeg Func Bool
            | Faster
            | Slower
            | IncEvSpd
            | DecEvSpd

getDisplay mdl func = case func of
  Dx -> mdl.dx
  Dx2 -> mdl.dx2
  H -> mdl.h
  Momentum -> mdl.momentum
  Speed -> mdl.speed
  Ampl -> mdl.ampl
  
setDisplay mdl func dsp = case func of
  Dx -> { mdl | dx = dsp }
  Dx2 -> { mdl | dx2 = dsp }
  H -> { mdl | h = dsp }
  Momentum -> { mdl | momentum = dsp }
  Speed -> { mdl | speed = dsp }
  Ampl -> { mdl | ampl = dsp }

main : Program Never
main = Html.program
    { init = (initialState, Cmd.none)
    , view = view
    , subscriptions = (\model -> Sub.batch [
                                      AnimationFrame.diffs NewFrame,
                                      Mouse.moves MouseMove,
                                      Mouse.downs (\_ -> MouseBttn True),
                                      Mouse.ups (\_ -> MouseBttn False)
                                     ] )
    , update = updateModel
    }

nextState a spd ndt = List.foldl (\i s -> evolveRK quantumState (0.0001* toFloat ndt) s) a [1..spd]

updateModel : Action -> Model -> (Model, Cmd Action)
updateModel msg model = 
  let mdl = case msg of
             MouseBttn down -> {model | drag = down}
             NewFrame dt -> {model | t = model.t + dt
                            , psy = if model.evolve 
                                       then nextState model.psy model.evolutionSpeed model.ndt
                                       else model.psy
                            }
             MouseMove p -> 
               if model.drag then               
                 {model | angx = model.angx - toFloat (p.x - model.oldpos.x),
                          angy = model.angy - toFloat (p.y - model.oldpos.y),
                          oldpos = p}
               else {model | oldpos = p}           
             Evolve b -> {model | evolve = b}
             PsyLeg b -> {model | psyLeg = b}
             Show func b -> let dsp = getDisplay model func 
                                dsp' = {dsp | show = b }
                            in setDisplay model func dsp'
             ShowLeg func b -> let dsp = getDisplay model func 
                                   dsp' = {dsp | leg = b }
                               in setDisplay model func dsp'
             IncEvSpd -> {model | evolutionSpeed = model.evolutionSpeed * 2 }
             DecEvSpd -> if model.evolutionSpeed >= 2 
                           then {model | evolutionSpeed = model.evolutionSpeed // 2       }
                           else model
             Faster -> { model | ndt = clamp 1 5 (model.ndt+1) }
             Slower -> { model | ndt = clamp 1 5 (model.ndt-1) }
  in (mdl, Cmd.none)

drawFun : (Int -> Complex) -> Bool -> Vec3 -> Mat4 -> List Renderable
drawFun f legs clr persp = 
  let points = [-50..49] |> List.map (\n -> 
        let x = toFloat n * 0.1 
            c = f n
        in (x,c))
      ball (x, c) = 
        render sphVertSh sphFragSh sphere { perspective = persp, pos = vec3 x c.re c.im, clr=clr }
      ballOnLeg (x, c) =
        let r = Complex.abs c
            a = arg c    
            tr = Math.Matrix4.identity 
                  |> translate3 x 0 0 
                  |> rotate a (vec3 1 0 0)
                  |> scale3 0.04 r 0.04 
            rot = Math.Matrix4.identity |> rotate a (vec3 1 0 0)              
            p = mul persp tr
            cy = render cylVertShdr cylFragShdr cylinder {perspective = p, pos = vec3 0 0 0, 
                                                          clr = clr, tfm = rot}
        in [ball (x, c), cy]                                                  
  in if legs then List.concatMap ballOnLeg points
             else List.map ball points

getQS : Int -> Array Complex -> Complex
getQS i a = case Array.get i a of
  Just v -> v
  Nothing -> Array.get (clamp 0 (Array.length a - 1) i) a |> Maybe.withDefault zero     

useQS : Array Complex -> Int -> Complex
useQS a i = getQS (i*5 + sizeQS // 2) a  -- for dx=0.02
-- useQS a i = getQS (i*10 + sizeQS // 2) a -- for dx=0.01

dxQS a = Array.indexedMap (\i v -> 
  let c = getQS (i+1) a `sub` getQS (i-1) a in {re = c.re / (2*dx), im = c.im / (2*dx)}) a
           
dx2QS a = Array.indexedMap (\i qx -> 
  let qleft = Maybe.withDefault zero (Array.get (i-1) a)
      qright = Maybe.withDefault zero (Array.get (i+1) a)
  in mult overdx2 ((qright `add` qleft) `sub` (mult c2 qx))) a

mapQS f a = Array.map f a 

webglView : Model -> Html msg
webglView mdl = 
  let persp = viewPersp mdl
      --psy = if mdl.evolve then oscillate mdl.psy mdl.t else mdl.psy
      psy = useQS mdl.psy           
      psydx _ = dxQS mdl.psy 
      psydx2 = dx2QS mdl.psy
      dx2 _ = psydx2  
      hpsy _ = mapQS (mult (complex -0.5 0)) psydx2 
      mmnt _ = mapQS (mult (complex 0 -1)) (psydx 0) 
      speed _ = mapQS (mult (complex 0 0.5)) psydx2  
      ampl _ = mapQS (\c -> complex (Complex.abs c) 0) mdl.psy
      draw f dsp clr = if dsp.show then drawFun (useQS (f 0)) dsp.leg clr persp else []
      --nxt = evolveRK quantumState 0.1 mdl.psy
      {-momentum x = mult (negation Complex.i) (psydx x)
      dpdt x = mult minusIdT (hpsy x)-}
  in WebGL.toHtml
          [ width 700, height 700, style [("backgroundColor", "#404060")] ]
          (List.concat [
             [axis persp] 
             , (drawFun psy mdl.psyLeg (vec3 0 1 0) persp)
             , (draw psydx mdl.dx (vec3 0 0 1)) 
             , (draw dx2 mdl.dx2 (vec3 0 0 0.5))
             , (draw hpsy mdl.h (vec3 1 0.5 0)) 
             , (draw mmnt mdl.momentum (vec3 1 0.5 0.5))
             , (draw speed mdl.speed (vec3 1 0 0))
             , (draw ampl mdl.ampl (vec3 0.5 0.5 0.5))
            ]
          )

styleTop = style [("vertical-align","top")]

displayCtrl mdl func caption clr =
  let dsp = getDisplay mdl func in
  [
      input [ type' "checkbox", checked dsp.show, onCheck (Show func) ] []
    , span [style [("color", clr)]] [text' caption]
    , input [ type' "checkbox", checked dsp.leg, onCheck (ShowLeg func) ] []
    , text "as vector"
    , br [] []
  ]  

controls mdl = 
  let ev = [
             input [ type' "checkbox", checked mdl.evolve, onCheck Evolve ] []
            , text' "evolve &nbsp; "
            , text' ("&nbsp;" ++ toString mdl.evolutionSpeed ++ "&nbsp; iterations per frame &nbsp;")
            , button [onClick IncEvSpd] [text "More"]
            , text' "&nbsp; &nbsp; "
            , button [onClick DecEvSpd] [text "Less"]
            , p [] []
            , text' ("dt = 0.000" ++ toString mdl.ndt ++ " &nbsp; ")
            , button [onClick Faster] [text "Faster"]
            , text' "&nbsp; &nbsp; "
            , button [onClick Slower] [text "Slower"]
            , p [] []
            , input [ type' "checkbox", checked mdl.psyLeg, onCheck PsyLeg ] []
            , text' "show &Psi; as vector"
            , br [] []
           ]
  in div [style [("color", "#c0c0c0")]] (List.concat 
              [ ev
              , displayCtrl mdl Dx "d/dx &Psi;   " "#0000FF"
              , displayCtrl mdl Dx2 "d2/dx2 &Psi;   " "#000080"
              , displayCtrl mdl H "Hamiltonian   " "#FF8000"
              , displayCtrl mdl Momentum "Momentum   " "#FF8080"
              , displayCtrl mdl Speed "d/dt &Psi;   " "#FF0000"
              , displayCtrl mdl Ampl "&Psi; amplitude   " "#808080"
              ])

view : Model -> Html Action
view mdl = 
  table [style [("backgroundColor", "#404060"), ("width", "100%")]] 
   [tr [] [td [style [("width", "700px")]] [webglView mdl],
           td [styleTop] [controls mdl]
          ]
   ]

viewPersp : Model -> Mat4
viewPersp m =
  let tfm = Math.Matrix4.identity |> rotate (m.angx/100) (vec3 0 1 0)  
                                  |> rotate (m.angy/100) (vec3 1 0 0)  
      campos = transform tfm (vec3 0 0 10)
  in mul (makePerspective 60 1 0.01 100)
         (makeLookAt campos (vec3 0 0 0) (vec3 0 1 0))


-- Time Evolution 

type alias DiffVecSpace v = {
  timeDerivative : v -> v,
  mulByFloat : Float -> v -> v,
  add : v -> v -> v
}

evolveRK : DiffVecSpace v -> Float -> v -> v
evolveRK ops dt state = 
  let a = ops.timeDerivative state
      b = ops.add state (ops.mulByFloat (dt/2) a) |> ops.timeDerivative
      c = ops.add state (ops.mulByFloat (dt/2) b) |> ops.timeDerivative
      d = ops.add state (ops.mulByFloat dt c) |> ops.timeDerivative
  in List.foldl ops.add state [ops.mulByFloat (dt/6) a,
                               ops.mulByFloat (dt/3) b,
                               ops.mulByFloat (dt/3) c,
                               ops.mulByFloat (dt/6) d]


addQS a b = Array.indexedMap (\i x -> x `add` (Maybe.withDefault zero (Array.get i b))) a

mulByFloatQS k a = let ck = complex k 0 in Array.map (mult ck) a


-- ih * d/dt = H   (Shr. Eq.)
-- using free particle Hamiltonian (m=1, h=1, H = -1/2 * d2/dx2), d/dt = i/2 * d2/dx2
timeDerQS a =
  Array.indexedMap (\i qx -> 
    let qleft = Maybe.withDefault zero (Array.get (i-1) a)
        qright = Maybe.withDefault zero (Array.get (i+1) a)
    in mult i2dx2 ((qright `add` qleft) `sub` (mult c2 qx))) a      

quantumState : DiffVecSpace (Array Complex)
quantumState = { add = addQS, mulByFloat = mulByFloatQS, timeDerivative = timeDerQS }

initQS : (Float -> Complex) -> Array Complex
initQS psy = Array.initialize sizeQS (\n -> psy (toFloat (n - (sizeQS // 2)) * dx))

-- Meshes

nX = 12
nY = 6

sphereDot : Float -> Float -> Vec3
sphereDot ix iy = let x = cos (ix * pi * 2 / nX) * sin (iy * pi / nY)
                      y = cos (iy * pi / nY)
                      z = sin (ix * pi * 2 / nX) * sin (iy * pi / nY)
                  in vec3 x y z

row0 y = List.map (\x -> (Vertex (sphereDot x y) (vec3 1 0 0),
                          Vertex (sphereDot (x+1) y) (vec3 0 1 0),
                          Vertex (sphereDot x (y-1)) (vec3 0 0 1))) [0..nX-1]

row1 y = List.map (\x -> (Vertex (sphereDot x y) (vec3 1 0 0),
                          Vertex (sphereDot (x-1) (y-1)) (vec3 0 1 0),
                          Vertex (sphereDot x (y-1)) (vec3 0 0 1))) [0..nX-1]

row y = List.append (if y < nY then row0 y else []) (if y > 1 then row1 y else [])

sphere : Drawable Vertex
sphere = Triangle (List.concatMap row [1..nY])


cylinder : Drawable Vertex
cylinder = let cyrow n = 
              let x = cos (n*2*pi/12)
                  z = sin (n*2*pi/12)
              in [Vertex (vec3 x 0 z) (vec3 x 0 z), Vertex (vec3 x 1 z) (vec3 x 0 z)]
           in [0..12] |> List.concatMap cyrow |> TriangleStrip 

axis : Mat4 -> Renderable
axis persp = 
  let t = Math.Matrix4.identity |> translate3 -10 0 0 |> scale3 20 0.08 0.08 
                  |> rotate (-pi/2) (vec3 0 0 1)
      rot = Math.Matrix4.identity |> rotate (-pi/2) (vec3 0 0 1)           
      p = mul persp t
  in render cylVertShdr cylFragShdr cylinder {perspective = p, pos = vec3 0 0 0, 
                                              clr = vec3 1 1 1, tfm = rot}

-- Shaders

sphVertSh : Shader { attr | position:Vec3, color:Vec3 } { unif | perspective:Mat4, pos:Vec3 } { v:Vec3 }
sphVertSh = [glsl| 

attribute vec3 position;
attribute vec3 color;
uniform mat4 perspective;
uniform vec3 pos;
varying vec3 v;

void main () {
    gl_Position = perspective * vec4(position*0.1 + pos, 1.0);
    v = position; 
}
|]

sphFragSh : Shader {} {u| clr:Vec3} { v:Vec3 }
sphFragSh = [glsl|

precision mediump float;
varying vec3 v;
uniform vec3 clr;

void main () {
    vec3 lightSource = vec3(-0.57735,  0.0, 0.816497);
    float q = dot(v, lightSource) / 2.0 + 0.5;
    float qq = q*q*q*q*q*q;
    vec3 c = q*clr + qq*vec3(1.0, 1.0, 1.0);
    gl_FragColor = vec4(c, 1.0);
}
|]

--here color attr really carries norm
cylVertShdr : Shader { attr | position:Vec3, color:Vec3 } { unif | perspective:Mat4, pos:Vec3 } { v:Vec3 }
cylVertShdr = [glsl|
precision mediump float;

attribute vec3 position;
attribute vec3 color;
uniform mat4 perspective;
uniform vec3 pos;
varying vec3 v;

void main () {
    gl_Position = perspective * vec4(position + pos, 1.0);
    v = color;
}
|]

cylFragShdr : Shader {} {u|clr:Vec3, tfm:Mat4} { v:Vec3 }
cylFragShdr = [glsl|

precision mediump float;
uniform vec3 clr;
uniform mat4 tfm;
varying vec3 v;

void main () {
    vec3 lightSource = vec3(-0.57735,  0.0, 0.816497);
    vec4 v2 = tfm * vec4(v, 1.0);
    float q = dot(v2.xyz, lightSource) / 2.0 + 0.5;
    vec3 c = q * clr;
    gl_FragColor = vec4(c, 1.0);
}
|]
