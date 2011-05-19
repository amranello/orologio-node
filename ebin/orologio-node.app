{application, 'orologio-node',
  [{description, "Node of monitoring system"},
   {vsn, "0.0.1"},
   {modules, ['orologio-node', 'orologio-node_app', 'orologio-node_sup', 'orologio-node_elem', mod_config_file, mod_log_file]},
   {applications, [kernel, stdlib, sasl]},
   {mod, {'orologio-node_app', []}}
  ]
}.
