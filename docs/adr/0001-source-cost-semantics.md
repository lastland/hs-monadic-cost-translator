# Preserve source demand and sharing under a fixed cost model

The translator will preserve the source program's call-by-need behavior and
instrument translated function bodies with one entry tick, rather than measure
GHC-optimized execution. Optimizations can change calls and sharing without
changing total output values, so they cannot determine the cost model used by
the paper's source-level properties.

Constructor applications and supported primitives on strict types receive no
ticks. Composition and partial-application adapters also receive no ticks;
functions they invoke retain their own ticks. Supported handwritten library
implementations retain their documented instrumentation. Operand computations
still contribute their costs when forced, even when the primitive operation is
unticked.

Source pattern and guard order takes precedence over superficial resemblance to
a handwritten example. In particular, the reference `takeM` translates a
guard-first implementation, not a source definition that matches the list first.
Reference cost tests must match both the source behavior and instrumentation.
