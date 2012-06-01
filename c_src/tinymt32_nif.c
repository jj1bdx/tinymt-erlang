/*
 * @file  tinymt32_nif.c
 * @brief TinyMT PRNG C NIF library for Erlang.
 *
 * @author Kenji Rikitake (Kyoto University)
 * @author Mutsuo Saito (Hiroshima University)
 * @author Makoto Matsumoto (Tokyo University)
 */

/*
 * (This is a simplified BSD license.)
 * 
 * Copyright (c) 2012 Kenji Rikitake and Kyoto University. All rights
 * reserved.
 * 
 * Copyright (c) 2011 Mutsuo Saito, Makoto Matsumoto, Hiroshima
 * University and The University of Tokyo. All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 *     * Neither the name of the Hiroshima University, The University of
 *       Tokyo, Kyoto University, nor the names of its contributors may be
 *       used to endorse or promote products derived from this software
 *       without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "erl_nif.h"

/* TinyMT definitions */

#include <inttypes.h>

#define TINYMT32_MEXP 127
#define TINYMT32_SH0 1
#define TINYMT32_SH1 10
#define TINYMT32_SH8 8
#define TINYMT32_MASK UINT32_C(0x7fffffff)
#define TINYMT32_MUL (1.0f / 4294967296.0f)

/**
 * tinymt32 internal state vector and parameters
 */
struct TINYMT32_T {
    uint32_t status[4];
    uint32_t mat1;
    uint32_t mat2;
    uint32_t tmat;
};

typedef struct TINYMT32_T tinymt32_t;

/** Version number for load_info. */
#define NIF_LOAD_INFO (101)

/* prototypes */
static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);
static int reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

static ERL_NIF_TERM tinymt32_nif_get_lib_refc(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tinymt32_nif_next_state(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tinymt32_nif_temper(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

inline static void tinymt32_next_state(tinymt32_t *random, tinymt32_t *random2);
inline static uint32_t tinymt32_temper(tinymt32_t *random);

/** Number of users of this dynamic library. */
static int lib_refc = 0;

/** Function list passed to the Erlang BEAM for this NIF. */
static ErlNifFunc nif_funcs[] = {
    {"get_lib_refc", 0, tinymt32_nif_get_lib_refc},
    {"next_state", 1, tinymt32_nif_next_state},
    {"temper", 1, tinymt32_nif_temper}
};

/* Function call macro to initialize NIF. */
ERL_NIF_INIT(tinymt32_nif, nif_funcs, load, reload, upgrade, unload)

/** An Erlang atom container. */
static ERL_NIF_TERM atom_error;
/** An Erlang atom container. */
static ERL_NIF_TERM atom_error1;
/** An Erlang atom container. */
static ERL_NIF_TERM atom_error2;
/** An Erlang atom container. */
static ERL_NIF_TERM atom_error3;
/** An Erlang atom container. */
static ERL_NIF_TERM atom_ok;

/**
 * Checks the version number of the load info from Erlang.
 * See is_ok_load_info() in c_src/crypto.c of the crypto module.
 * @param env ErlNifEnv pointer for the calling process.
 * @param load_info ERL_NIF_TERM to identify the NIF library.
 */
static int check_load_info(ErlNifEnv* env, ERL_NIF_TERM load_info)
{
    int i;

    /* check the version number of the load info */
    return enif_get_int(env,load_info,&i) && 
	i == NIF_LOAD_INFO;
}

/**
 * Loads NIF module and defines Erlang atoms.
 * @param env ErlNifEnv pointer for the calling process.
 * @param priv_data pointing the private data for the NIF library to keep between the NIF calls.
 * @param load_info ERL_NIF_TERM to identify the NIF library.
 */
static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    /* checking version number on the argument load_info */
    if (!check_load_info(env, load_info)) {
	return -1;
    }

    /* initializing atoms */
    atom_error = enif_make_atom(env,"error");
    atom_error1 = enif_make_atom(env,"error1");
    atom_error2 = enif_make_atom(env,"error2");
    atom_error3 = enif_make_atom(env,"error3");
    atom_ok = enif_make_atom(env,"ok");

    *priv_data = NULL;

    /* increase the reference count of this library */
    lib_refc++;

    return 0;
}

/**
 * Reloads NIF module.
 * @param env ErlNifEnv pointer for the calling process.
 * @param priv_data pointing the private data for the NIF library to keep between the NIF calls.
 * @param load_info ERL_NIF_TERM to identify the NIF library.
 */
static int reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    /* Don't know how to do this */
    if (*priv_data != NULL) {
	return -1; 
    }

    /* No support for real library upgrade.
       The tricky thing is to know
       when to (re)set the callbacks for allocation and locking. */
    if (lib_refc == 0) {
	return -2;
    }

    /* checking version number on the argument load_info */
    if (!check_load_info(env, load_info)) {
	return -1;
    }

    return 0;
}

/**
 * Upgrades NIF module.
 * @param env ErlNifEnv pointer for the calling process.
 * @param priv_data pointing the private data for the NIF library to keep between the NIF calls.
 * @param old_priv_data pointing the private data given from the last calls of load() or reload().
 * @param load_info ERL_NIF_TERM to identify the NIF library.
 */
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    int i;

    /* Don't know how to do this */
    if (*old_priv_data != NULL) {
	return -1;
    }

    /* reloading first */
    i = reload(env, priv_data, load_info);

    /* return the error code from reload() if necessary */
    if (0 != i) {
	return i;
    }

    /* increase the reference count of this library */
    lib_refc++;

    return 0;
}

/**
 * Unloads NIF module.
 * @param env ErlNifEnv pointer for the calling process.
 * @param priv_data pointing the private data for the NIF library to keep between the NIF calls.
 */
static void unload(ErlNifEnv* env, void* priv_data)
{
    /* this is yet a skeleton code */
    /* perform resource unlock and deallocation here,
       but in this module no resource are retained
       outside of the scope of each function */
    if (--lib_refc <= 0) {
	/* do nothing */
    }
    /*else NIF library still used by other (new) module code */
}

static ERL_NIF_TERM
tinymt32_nif_next_state(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{ /* (#intstate32{}, 8 element tuple) */
    tinymt32_t old, new;
    int arity;
    const ERL_NIF_TERM *elements;

    if (!enif_get_tuple(env, argv[0], &arity, &elements)
        || arity != 8 ) {
	return enif_make_badarg(env);
    }

    if (!enif_is_atom(env, elements[0])
	|| !enif_get_uint(env, elements[1], &old.status[0])
	|| !enif_get_uint(env, elements[2], &old.status[1])
	|| !enif_get_uint(env, elements[3], &old.status[2])
	|| !enif_get_uint(env, elements[4], &old.status[3])
	|| !enif_get_uint(env, elements[5], &old.mat1)
	|| !enif_get_uint(env, elements[6], &old.mat2)
	|| !enif_get_uint(env, elements[7], &old.tmat)) {
	return enif_make_badarg(env);
    }

    new.mat1 = old.mat1;
    new.mat2 = old.mat2;
    new.tmat = old.tmat;
    tinymt32_next_state(&old, &new);

    return enif_make_tuple8(env,
			    elements[0],
			    enif_make_uint(env, new.status[0]),
			    enif_make_uint(env, new.status[1]),
			    enif_make_uint(env, new.status[2]),
			    enif_make_uint(env, new.status[3]),
			    enif_make_uint(env, new.mat1),
			    enif_make_uint(env, new.mat2),
			    enif_make_uint(env, new.tmat));

}

static ERL_NIF_TERM
tinymt32_nif_temper(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{ /* (#intstate32{}, 8 element tuple) */
    tinymt32_t state;
    int arity;
    const ERL_NIF_TERM *elements;
    uint32_t n;

    if (!enif_get_tuple(env, argv[0], &arity, &elements)
        || arity != 8 ) {
	return enif_make_badarg(env);
    }

    if (!enif_is_atom(env, elements[0])
	|| !enif_get_uint(env, elements[1], &state.status[0])
	|| !enif_get_uint(env, elements[2], &state.status[1])
	|| !enif_get_uint(env, elements[3], &state.status[2])
	|| !enif_get_uint(env, elements[4], &state.status[3])
	|| !enif_get_uint(env, elements[5], &state.mat1)
	|| !enif_get_uint(env, elements[6], &state.mat2)
	|| !enif_get_uint(env, elements[7], &state.tmat)) {
	return enif_make_badarg(env);
    }

    n = tinymt32_temper(&state);

    return enif_make_uint(env, n);

}

/**
 * NIF code of get_lib_refc/0.
 * @param env ErlNifEnv pointer for the calling process.
 * @param argc Erlang function arity.
 * @param argv ERL_NIF_TERM pointers for the arguments.
 */
static ERL_NIF_TERM
tinymt32_nif_get_lib_refc(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{ /* () */
    return enif_make_int(env, lib_refc);
}

/* TinyMT static C function code follows */


/**
 * This function changes internal state of tinymt32.
 * Users should not call this function directly.
 * @param random tinymt internal status
 */

inline static void
tinymt32_next_state(tinymt32_t *random, tinymt32_t *random2) {
    uint32_t x;
    uint32_t y;

    y = random->status[3];
    x = (random->status[0] & TINYMT32_MASK)
        ^ random->status[1]
        ^ random->status[2];
    x ^= (x << TINYMT32_SH0);
    y ^= (y >> TINYMT32_SH0) ^ x;
    random2->status[0] = random->status[1];
    random2->status[1] = random->status[2];
    random2->status[2] = x ^ (y << TINYMT32_SH1);
    random2->status[3] = y;
    random2->status[1] ^= -((int32_t)(y & 1)) & random->mat1;
    random2->status[2] ^= -((int32_t)(y & 1)) & random->mat2;
}

/**
 * This function outputs 32-bit unsigned integer from internal state.
 * Users should not call this function directly.
 * @param random tinymt internal status
 * @return 32-bit unsigned pseudorandom number
 */
inline static uint32_t tinymt32_temper(tinymt32_t *random) {
    uint32_t t0, t1;

    t0 = random->status[3];
    t1 = random->status[0]
        + (random->status[2] >> TINYMT32_SH8);
    t0 ^= t1;
    t0 ^= -((int32_t)(t1 & 1)) & random->tmat;
    return t0;
}

