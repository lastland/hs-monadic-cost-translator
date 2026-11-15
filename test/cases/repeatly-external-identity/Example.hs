module Example where
import Memorist.Example.NewtonRaphson (repeatly)
step x = x + x
repeated x = head (tail (repeatly step x))
