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

#ifndef GOLOMBRICE_H
#define GOLOMBRICE_H

#include "bitstream.h"

#define GOLOMBRICE_VERSION "0.1.0"

/// Write only stream of golomb rice encoded bits
struct golombrice_writer_t {
    /// Written bits
    struct bitstream_writer_t stream;
    /// Number of bits P in remainder part of encoding
    int p;
};

/// Read only stream of golomb rice encoded bits
struct golombrice_reader_t {
    /// Reading bits
    struct bitstream_reader_t stream;
    /// Number of bits P in remainder part of encoding
    int p;
};

/*
 * The writer.
 */

/** Allocate new writer structure.
*
* It allocates only control structure and need to be initialized
* with golombrice_writer_init.
*
* Destroy it with golombrice_writer_delete.
*/
struct golombrice_writer_t* golombrice_writer_new();

/** Deallocates memory for control structure of golomb rice stream.
*
* Note that it doesn't destroy underlying buffer for data.
*/
void golombrice_writer_delete(struct golombrice_writer_t *self_p);

/** Initialize the writer with given buffer and encoding parameter.
*
*  The p parameter defines number of bits in remainder part of encoding.
*/
void golombrice_writer_init(struct golombrice_writer_t *self_p,
                            uint8_t *buf_p, int p);

/// Return amount of bytes written into the stream.
int golombrice_writer_length(struct golombrice_writer_t *self_p);

/// Replace buffer of the stream without changing it state
void golombrice_writer_update_buffer(struct golombrice_writer_t *self_p, uint8_t *buf_p);

/// Return encoded data into the stream.
uint8_t* golombrice_writer_data(struct golombrice_writer_t *self_p);

/// Encodes single word into the stream.
void golombrice_writer_encode_word(struct golombrice_writer_t *self_p,
                                   uint64_t v);

/// Encodes array of words into the stream.
void golombrice_writer_encode_words(struct golombrice_writer_t *self_p,
                                   uint64_t *words, int n);

/*
 * The reader.
 */

/** Allocate new writer structure.
*
* It allocates only control structure and need to be initialized
* with golombrice_writer_init.
*
* Destroy it with golombrice_writer_delete.
*/
struct golombrice_reader_t* golombrice_reader_new();

/** Deallocates memory for control structure of golomb rice stream.
*
* Note that it doesn't destroy underlying buffer for data.
*/
void golombrice_reader_delete(struct golombrice_reader_t *self_p);

/** Initialize the reader with given buffer and encoding parameter.
*
*  The p parameter defines number of bits in remainder part of encoding.
*/
void golombrice_reader_init(struct golombrice_reader_t *self_p,
                           uint8_t *buf_p, int p);

/// Returns length in bytes that remains in the stream.
int golombrice_reader_length(struct golombrice_reader_t *self_p);

/// Decodes next word from the stream. Undefined behavior on empty stream.
uint64_t golombrice_reader_decode_word(struct golombrice_reader_t *self_p);

/// Decodes given amount of words into buffer. Undefined behavior if
/// there is no enough bytes inside the stream.
void golombrice_reader_decode_words(struct golombrice_reader_t *self_p,
                                    uint64_t *words, int n);


#endif
