/*---------------------------------------------------------------------------
   Copyright (c) 2019 The down programmers. All rights reserved.
   SPDX-License-Identifier: ISCe.
   %%NAME%% release %%VERSION%%
   --------------------------------------------------------------------------*/

#include <stdbool.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>

#ifdef _WIN32

#include <Windows.h>

static HANDLE hInput = INVALID_HANDLE_VALUE;
static HANDLE hOutput = INVALID_HANDLE_VALUE;
static HANDLE hError = INVALID_HANDLE_VALUE;

CAMLprim value ocaml_down_stdin_set_raw_mode (value set_raw)
{
  static DWORD hOrigInputMode, hOrigOutputMode, hOrigErrorMode;
  static bool is_raw = FALSE;
  DWORD hRawInputMode = ENABLE_VIRTUAL_TERMINAL_INPUT | ENABLE_WINDOW_INPUT;
  DWORD hRawOutputMode = ENABLE_PROCESSED_OUTPUT | ENABLE_VIRTUAL_TERMINAL_PROCESSING;

  if (hInput == INVALID_HANDLE_VALUE) hInput = GetStdHandle(STD_INPUT_HANDLE);
  if (hOutput == INVALID_HANDLE_VALUE) hOutput = GetStdHandle(STD_OUTPUT_HANDLE);
  if (hError == INVALID_HANDLE_VALUE) hError = GetStdHandle(STD_ERROR_HANDLE);

  if (Bool_val(set_raw)) {
    if (!is_raw) {
      if (GetConsoleMode(hInput, &hOrigInputMode) == 0 ||
          GetConsoleMode(hOutput, &hOrigOutputMode) == 0 ||
          GetConsoleMode(hError, &hOrigErrorMode) == 0)
        return Val_bool(0);

      if (SetConsoleMode(hInput, hRawInputMode) == 0)
        return Val_bool(0);

      if (SetConsoleMode(hOutput, hRawOutputMode) == 0) {
        SetConsoleMode(hInput, hOrigInputMode);
        return Val_bool(0);
      }
      if (SetConsoleMode(hError, hRawOutputMode) == 0) {
        SetConsoleMode(hInput, hOrigInputMode);
        SetConsoleMode(hOutput, hOrigOutputMode);
        return Val_bool(0);
      }
      is_raw = TRUE;
    }
  } else {
    if (is_raw) {
      if (SetConsoleMode(hInput, hOrigInputMode | hRawInputMode) == 0 ||
          SetConsoleMode(hOutput, hOrigOutputMode | hRawOutputMode) == 0 ||
          SetConsoleMode(hInput, hOrigErrorMode | hRawOutputMode) == 0) {
        return Val_bool(0);
      }

      is_raw = FALSE;
    }
  }

  return Val_bool(1);
}

CAMLprim value ocaml_down_stdin_readc (value unit)
{
  char buf;

  if (hInput == INVALID_HANDLE_VALUE) hInput = GetStdHandle(STD_INPUT_HANDLE);

  if (ReadFile(hInput, &buf, 1, NULL, NULL) == FALSE)
    return Val_int(-3);

  return Val_int(buf);
}

CAMLprim value ocaml_down_sigwinch (value unit)
{
  return Val_int(0);
}

#else

#include <termios.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>

CAMLprim value ocaml_down_stdin_set_raw_mode (value set_raw)
{
  CAMLparam1 (set_raw);
  static bool is_raw = false;
  static struct termios restore = {0};
  struct termios set;

  if (Bool_val (set_raw)) {
    if (!is_raw) {
      if (!isatty (0)) { CAMLreturn (Val_bool (0)); }
      if (tcgetattr (0, &restore) < 0) { CAMLreturn (Val_bool (0)); }
      set = restore;
      set.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
      set.c_oflag &= ~(OPOST);
      set.c_cflag |= (CS8);
      set.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
      set.c_cc[VMIN] = 1; set.c_cc[VTIME] = 0;
      if (tcsetattr (0, TCSAFLUSH, &set) < 0) { CAMLreturn (Val_bool (0)); }
      else { is_raw = true; };
    }
  } else {
    if (is_raw) {
      if (tcsetattr (0, TCSAFLUSH, &restore) < 0) { CAMLreturn (Val_bool (0)); }
      else { is_raw = false; }
    }
  }
  CAMLreturn (Val_bool (1));
}

CAMLprim value ocaml_down_stdin_readc (value unit)
{
  CAMLparam1 (unit);
  int ret; unsigned char buf;
  ret = read(0, &buf, 1);
  if (ret == 1) { CAMLreturn (Val_int (buf)); };
  if (ret == 0) { CAMLreturn (Val_int (-1)); };
  if (ret == -1 && errno == EINTR) { CAMLreturn (Val_int (-2)); };
  CAMLreturn (Val_int (-3));
}

CAMLprim value ocaml_down_sigwinch (value unit)
{
  CAMLparam1 (unit);
  CAMLreturn (Val_int (SIGWINCH));
}

#endif
