module Main where

import Generated
import ReferenceSupport

probes :: [Probe]
probes = [scalar "value" (const undefined)]

main = pure ()
