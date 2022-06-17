
- Implement bracketed paste. Improves responsiveness
  on large code pastes (#16). Thanks to Etienne Millon
  for the suggestion and Hannes Mehnert for the report.

v0.1.0 2022-01-26 La Forclaz (VS)
---------------------------------

- Support for OCaml 4.14. Thanks to Kate (@kit-ty-kate) for 
  the patch.
- Build system: use `opam var` instead of `opam config var`.

v0.0.4 2022-01-26 La Forclaz (VS)
---------------------------------

- Upgrade Unicode TTY width data to 14.0.0.

v0.0.3 2020-07-12 Zagreb
------------------------

- Fix crash on get type of empty input. Thanks to @kakadu for the report.

v0.0.2 2019-08-15 Zagreb
------------------------

- Identifier documentation access (`C-t`): allow cursor to be on the
  whitespace or non-identifier characters after the identifier.
  Thanks to Zeng Li.
- Be more subtle when line edition is not available. Let the Down
  API work reasonably well and/or error gracefully (#17).
- Less clever, but more robust, end of user input detection (#14).
- Tweak configuration directory lookup on Windows.

v0.0.1 2019-07-23 Zagreb
------------------------

First release. 
