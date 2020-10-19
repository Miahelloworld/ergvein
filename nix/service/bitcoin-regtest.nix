{ config, pkgs, lib, ... }:
with lib;  # use the functions from lib, such as mkIf
let
  reflex-platform = import ../reflex-platform.nix {};
  # the values of the options set for the service by the user of the service
  cfg = config.services.bitcoin-regtest;
  # Script to call local bitcoin node
  local-cli-script = pkgs.writeShellScriptBin "regtest-bitcoin-cli" ''
    export RPC_PASSWORD=$(cat ${cfg.passwordFile} | xargs echo -n)
    ${cfg.package}/bin/bitcoin-cli -datadir=${cfg.datadir} -conf=/etc/${cfg.configPath} -rpcpassword=$RPC_PASSWORD "$@"
  '';
in {
  ##### interface. here we define the options that users of our service can specify
  options = {
    # the options for our service will be located under services.bitcoin-regtest
    services.bitcoin-regtest = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Whether to enable bitcoin node in regtest mode by default.
        '';
      };
      package = mkOption {
        type = types.package;
        default = pkgs.bitcoind;
        defaultText = "pkgs.bitcoind";
        description = ''
          Which bitcoin package to use with the service. The bitcoin node have to
          be built with ZeroMQ support.
        '';
      };
      nodeUser = mkOption {
        type = types.str;
        default = "bitcoin";
        description = ''
          Which name of RPC user to use.
        '';
      };
      nodePort = mkOption {
        type = types.int;
        description = ''
          Which port the cryptonode serves RPC.
        '';
      };
      nodeAddress = mkOption {
      	type = types.str;
        default = "127.0.0.1";
        description = ''
          Which address to listhen for node RPC.
        '';
      };
      datadir = mkOption {
        type = types.str;
        default = "/var/lib/bitcoin-regtest";
        description = ''
          Path to blockchain database on filesystem.
        '';
      };
      config = mkOption {
        type = types.str;
        default = ''
          regtest=1
          dnsseed=0
          upnp=0
          server=1
          rest=1
          txindex=1
          [regtest]
          port=19000
          rpcallowip=${cfg.nodeAddress}
          rpcuser=${cfg.nodeUser}
          rpcport=${toString cfg.nodePort}
        '';
        description = ''
          Configuration file for bitcoin.
        '';
      };
      configPath = mkOption {
        type = types.str;
        default = "bitcoin-regtest.conf";
        description = ''
          Configuration file location for bitcoin relative to /etc.
        '';
      };
      passwordFile = mkOption {
        type = types.str;
        default = "/run/keys/btcpassword";
        description = ''
          Location of file with password for RPC.
        '';
      };
      passwordFileService = mkOption {
        type = types.str;
        default = "btcpassword-key.service";
        description = ''
          Service that indicates that passwordFile is ready.
        '';
      };
    };
  };

  ##### implementation
  config = mkIf cfg.enable { # only apply the following settings if enabled
    # Write configuration file to /etc/bitcoin.conf
    environment.etc."${cfg.configPath}" = {
      text = cfg.config; # we can use values of options for this service here
    };
    # Write shortcut script to run commands on the node
    environment.systemPackages = [
      local-cli-script
    ];
    # Create systemd service
    systemd.services.bitcoin-regtest = {
      enable = true;
      description = "Bitcoin regtest node";
      after = ["network.target" cfg.passwordFileService];
      wants = ["network.target" cfg.passwordFileService];
      script = ''
        export RPC_PASSWORD=$(cat ${cfg.passwordFile} | xargs echo -n)
        ${cfg.package}/bin/bitcoind -datadir=${cfg.datadir} -conf=/etc/${cfg.configPath} -deprecatedrpc=accounts -fallbackfee=100 -rpcpassword=$RPC_PASSWORD
      '';
      serviceConfig = {
          Restart = "always";
          RestartSec = 30;
          User = "root";
        };
      wantedBy = ["multi-user.target"];
    };
    # Init folder for bitcoin data
    system.activationScripts = {
      intbitcoin-regtest = {
        text = ''
          if [ ! -d "${cfg.datadir}" ]; then
            mkdir -p ${cfg.datadir}
          fi
        '';
        deps = [];
      };
    };
  };
}
