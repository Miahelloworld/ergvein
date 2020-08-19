/**
 * The MIT License (MIT)
 *
 * Copyright (c) 2020 Anton Gushcha
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <stdlib.h>
#include <math.h>

#include "golombrice.h"

struct golombrice_writer_t* golombrice_writer_new(int buff_size, int p) {
  struct golombrice_writer_t* wr = (struct golombrice_writer_t*)malloc(sizeof(struct golombrice_writer_t));
  wr->p = p;
  uint8_t *buf_p = (uint8_t*)malloc(buff_size);
  bitstream_writer_init(&wr->stream, buf_p);
  return wr;
}

void golombrice_writer_delete(struct golombrice_writer_t *self_p) {
  free(self_p->stream.buf_p);
  free(self_p);
}

int golombrice_writer_length(struct golombrice_writer_t *self_p) {
  return bitstream_writer_size_in_bytes(&self_p->stream);
}

uint8_t* golombrice_writer_data(struct golombrice_writer_t *self_p) {
  return self_p->stream.buf_p;
}

void golombrice_writer_ensure(struct golombrice_writer_t *self_p, int n) {
  int l = bitstream_writer_size_in_bytes(&self_p->stream);
  if (self_p->size <= l+n) {
    int n2 = self_p->size * 2;
    if (n2 < l+n) n2 = l+n;
    self_p->stream.buf_p = realloc(self_p->stream.buf_p, n2);
  }
}

void golombrice_writer_encode_word(struct golombrice_writer_t *self_p,
                                   uint64_t v) {
  int p = self_p->p;
  uint64_t q = v >> p;
  int n = (int)ceil((double)(q + 1 + p) / 8.0);
  golombrice_writer_ensure(self_p, n);
  bitstream_writer_write_repeated_bit(&self_p->stream, 1, q);
  bitstream_writer_write_bit(&self_p->stream, 0);
  uint64_t x = (v << p) >> p; // clear all unless p lower bits
  bitstream_writer_write_u64_bits(&self_p->stream, x, p);
}

void golombrice_writer_encode_words(struct golombrice_writer_t *self_p,
                                   uint64_t *words, int n) {
  golombrice_writer_ensure(self_p, n*8); // estimation that encoding will be less than original
  for(int i = 0; i < n; i++) {
    golombrice_writer_encode_word(self_p, words[i]);
  }
}
