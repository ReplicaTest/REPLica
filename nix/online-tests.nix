{
  name = "REPLica online tests";
  description =
    ''
      We don't run online tests in the CI, so we check
      it in pre-commit
    '';
  enable = true;
  entry =
    ''
      make test RUN="-t online"
    '';
  pass_filenames = false;
  stages = [ "push" ];
}
