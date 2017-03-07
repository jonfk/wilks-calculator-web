module Wilks exposing (..)

maleWilksCoeffKg : Float -> Float
maleWilksCoeffKg bodyweight =
  let
    a = -216.0475144
    b = 16.2606339
    c = -0.002388645
    d = -0.00113732
    e = 0.00000701863
    f = -0.00000001291
  in
    wilksCoeffWithCoeffs a b c d e f bodyweight

femaleWilksCoeffKg : Float -> Float
femaleWilksCoeffKg bodyweight =
  let
    a = 594.31747775582
    b = -27.23842536447
    c = 0.82112226871
    d = -0.00930733913
    e = 0.00004731582
    f = -0.00000009054
  in
    wilksCoeffWithCoeffs a b c d e f bodyweight

wilksCoeffWithCoeffs : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
wilksCoeffWithCoeffs a b c d e f bodyweight =
  let
    x = bodyweight
  in
    500 / (a + (b * x) + (c * (x^2)) + (d * (x^3)) + (e * (x^4)) + (f * (x^5)))
