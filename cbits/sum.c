#include <emmintrin.h>

int32_t sum_vec_i32(const int32_t vec[], const int vecLength) {
  int rest = vecLength % 4;

  // four partial sums
  __m128i vsum = _mm_set1_epi32(0);

  int32_t sum;
  int i;

  for (i = 0; i < vecLength - rest; i += 4) {
    __m128i v = _mm_load_si128(&vec[i]);
    vsum = _mm_add_epi32(vsum, v);
  }

  vsum = _mm_add_epi32(vsum, _mm_srli_si128(vsum, 8));
  vsum = _mm_add_epi32(vsum, _mm_srli_si128(vsum, 4));
  sum = _mm_cvtsi128_si32(vsum);

  for (; i < vecLength; i++) {
    sum += vec[i];
  }

  return sum;
}


int64_t sum_vec_i64(const int64_t vec[], const int vecLength) {
  int rest = vecLength % 2;

  // two partial sums
  int64_t init = 0;
  __m128i vsum = _mm_set1_epi64((__m64)init);

  int64_t sum;
  int i;

  for (i = 0; i < vecLength - rest; i += 2) {
    __m128i v = _mm_load_si128(&vec[i]);
    vsum = _mm_add_epi64(vsum, v);
  }

  vsum = _mm_add_epi64(vsum, _mm_srli_si128(vsum, 8));
  sum = _mm_cvtsi128_si64(vsum);

  for (; i < vecLength; i++) {
    sum += vec[i];
  }

  return sum;
}

double sum_vec_dbl(const double vec[], const int vecLength) {
  int rest = vecLength % 2;

  // two partial sums
  double init = 0;
  __m128d vsum = _mm_set1_pd(init);

  double sum;
  int i;

  for (i = 0; i < vecLength - rest; i += 2) {
    __m128d v = _mm_load_pd(&vec[i]);
    vsum = _mm_add_pd(vsum, v);
  }

  vsum = _mm_add_pd(vsum, (__m128d)_mm_srli_si128((__m128i)vsum, 8));
  sum = _mm_cvtsd_f64(vsum);

  for (; i < vecLength; i++) {
    sum += vec[i];
  }

  return sum;
}
