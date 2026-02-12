Temporary limitations

- peek-bytes, peek-bytes!, peek-string, peek-string! currently do not accept the 2-argument form.
  Reason: the default argument is (current-input-port), which is not implemented in the runtime yet.
