# vector-simd

## Benchmarks `Int32`

```
benchmarking main/sum/vector implementation
time                 519.0 ns   (518.4 ns .. 519.8 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 518.8 ns   (518.4 ns .. 519.3 ns)
std dev              1.559 ns   (1.243 ns .. 2.118 ns)

benchmarking main/sum/simd implementation
time                 121.1 ns   (121.0 ns .. 121.2 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 121.1 ns   (121.1 ns .. 121.2 ns)
std dev              237.2 ps   (137.7 ps .. 369.8 ps)
```


## Full Benchmarks

`Storable` SIMD uses FFI and C, while `Primitive` uses `GHC.Prim` SIMD and LLVM
backend.

Benchmarks done using 128 SIMD vectors on i7-3740QM


```
Benchmark vector-simd-bench: RUNNING...
benchmarking Int32/Sum Storable/vector implementation
time                 578.2 ns   (577.0 ns .. 579.7 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 580.4 ns   (579.1 ns .. 581.5 ns)
std dev              3.974 ns   (3.396 ns .. 4.918 ns)

benchmarking Int32/Sum Storable/simd implementation
time                 168.4 ns   (168.0 ns .. 168.7 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 167.9 ns   (167.6 ns .. 168.3 ns)
std dev              1.216 ns   (1.048 ns .. 1.472 ns)

benchmarking Int32/Sum Primitive/vector implementation
time                 585.5 ns   (582.8 ns .. 587.8 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 583.2 ns   (582.0 ns .. 584.4 ns)
std dev              4.107 ns   (3.303 ns .. 5.480 ns)

benchmarking Int32/Sum Primitive/simd implementation
time                 163.0 ns   (162.6 ns .. 163.4 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 163.1 ns   (162.7 ns .. 163.4 ns)
std dev              1.074 ns   (894.2 ps .. 1.392 ns)

benchmarking Int64/Sum Storable/vector implementation
time                 443.5 ns   (439.3 ns .. 448.7 ns)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 431.7 ns   (426.5 ns .. 437.6 ns)
std dev              17.49 ns   (14.40 ns .. 25.96 ns)
variance introduced by outliers: 58% (severely inflated)

benchmarking Int64/Sum Storable/simd implementation
time                 191.0 ns   (186.5 ns .. 195.2 ns)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 187.8 ns   (186.8 ns .. 189.9 ns)
std dev              4.587 ns   (2.593 ns .. 7.114 ns)
variance introduced by outliers: 35% (moderately inflated)

benchmarking Int64/Sum Primitive/vector implementation
time                 429.8 ns   (424.4 ns .. 437.4 ns)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 443.3 ns   (438.9 ns .. 446.9 ns)
std dev              13.69 ns   (10.72 ns .. 16.27 ns)
variance introduced by outliers: 45% (moderately inflated)

benchmarking Int64/Sum Primitive/simd implementation
time                 330.3 ns   (327.2 ns .. 331.9 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 320.5 ns   (317.3 ns .. 324.0 ns)
std dev              11.16 ns   (10.43 ns .. 11.79 ns)
variance introduced by outliers: 51% (severely inflated)

benchmarking Double/Sum Storable/vector implementation
time                 854.6 ns   (852.2 ns .. 856.9 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 863.0 ns   (857.2 ns .. 870.7 ns)
std dev              22.34 ns   (15.82 ns .. 27.23 ns)
variance introduced by outliers: 35% (moderately inflated)

benchmarking Double/Sum Storable/simd implementation
time                 458.4 ns   (449.5 ns .. 473.4 ns)
                     0.995 R²   (0.987 R² .. 0.999 R²)
mean                 468.6 ns   (463.4 ns .. 485.4 ns)
std dev              28.01 ns   (10.22 ns .. 54.97 ns)
variance introduced by outliers: 75% (severely inflated)

benchmarking Double/Sum Primitive/vector implementation
time                 851.7 ns   (849.1 ns .. 855.3 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 856.7 ns   (854.3 ns .. 858.3 ns)
std dev              6.546 ns   (5.526 ns .. 8.181 ns)

benchmarking Double/Sum Primitive/simd implementation
time                 428.8 ns   (427.9 ns .. 429.8 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 427.9 ns   (426.9 ns .. 429.1 ns)
std dev              3.714 ns   (2.807 ns .. 5.095 ns)

```
