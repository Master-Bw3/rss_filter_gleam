packages: { lib, config, pkgs, ... }:
let
  cfg = config.services.rssFilter;
in
{
  options.services.rssFilter = {
    enable = lib.mkEnableOption "Enable Module";
    package = lib.mkPackageOption packages.${pkgs.system} "rssFilter" { };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.rssFilter = {
      wantedBy = [ "multi-user.target" ];
      description = "Start the rss filter server.";

      serviceConfig = {
        Type = "simple";
        StateDirectory = "rss-filter";
        ExecStart = "${cfg.package}/bin/rss_filter_gleam";
        Restart = "always";
        User = "rssFilter";
        Group = "rssFilter";
      };
    };

    users.users.rssFilter = {
      group = "rssFilter";
      isSystemUser = true;
    };
    users.groups.rssFilter = { };
  };
}
