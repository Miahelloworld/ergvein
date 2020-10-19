{config, pkgs, lib, ...}:
with lib;
let
  project = import ../default.nix { minimize = true; };
  # the values of the options set for the service by the user of the service
  cfg = config.services.ergvein-faucet;
in {
  ##### interface. here we define the options that users of our service can specify
  options = {
    services.ergvein-faucet = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Whether to enable ergvein-faucet by default.
        '';
      };
      config = mkOption {
        type = types.str;
        description = ''
          Configuration file for backend server.
        '';
      };
      nodePasswordFile = mkOption {
        type = types.str;
        default = "/run/keys/regtestpassword";
        description = ''
          Location of file with password for RPC.
        '';
      };
      nodePasswordEnv = mkOption {
        type = types.str;
        default = "RPC_PASSWORD";
        description = ''
          Name of env variable with password for RPC.
        '';
      };
      nodePasswordFileService = mkOption {
        type = types.str;
        default = "regtestpassword-key.service";
        description = ''
          Service that indicates that nodePasswordFile is ready.
        '';
      };
    };
  };

  ##### implementation
  config = mkIf cfg.enable { # only apply the following settings if enabled
    environment.etc."ergvein-faucet.yaml" = {
      text = cfg.config;
    };
    # Create systemd service
    systemd.services.ergvein-faucet = {
      enable = true;
      description = "Xenochain faucet";
      after = ["network.target" cfg.nodePasswordFileService];
      wants = ["network.target" cfg.nodePasswordFileService];
      script = ''
        export ${cfg.nodePasswordEnv}=$(cat ${cfg.nodePasswordFile} | xargs echo -n)
        ${project.ghc.ergvein-faucet-back}/bin/ergvein-faucet /etc/ergvein-faucet.yaml
      '';
      serviceConfig = {
          Restart = "always";
          RestartSec = 30;
          User = "root";
        };
      wantedBy = ["multi-user.target"];
    };
  };
}
