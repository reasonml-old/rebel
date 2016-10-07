/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
open Core.Std;

open Yojson.Basic;

open Jenga_lib;

/*
  =====================================================
    COPIED CODE BEGIN
  =====================================================
 */
let terminal_type =
  switch (Core.Std.Sys.getenv "TERM") {
  | None => ""
  | Some x => x
  };

let module Spec = Command.Spec;

let module Param = Command.Param;

let (%:) = Spec.(%:);

let j_number = {
  let (formula, default) =
    switch System.num_cpus_if_known {
    | None => ("", 1)
    | Some cpus => (" = max (#cpus-1) 1", Int.max (cpus - 1) 1)
    };
  Param.flag
    "j"
    (Spec.optional_with_default default Spec.int)
    doc::(sprintf "<jobs> parallel jobs, def: %d%s" default formula)
};

let f_number = {
  let (formula, default) =
    switch System.num_cpus_if_known {
    | None => ("", 1)
    | Some cpus => (" = (#cpus-1)/4 + 1", (cpus - 1) / 4 + 1)
    };
  Param.flag
    "f"
    (Spec.optional_with_default default Spec.int)
    doc::(sprintf "<forkers> forker processes, def: %d%s" default formula)
};

let d_number = {
  let (formula, default) =
    /* same defaults as for -j */
    switch System.num_cpus_if_known {
    | None => ("", 1)
    | Some cpus => (" = max (#cpus-1) 1", Int.max (cpus - 1) 1)
    };
  Param.flag
    "max-par-digest"
    (Spec.optional_with_default default Spec.int)
    doc::(sprintf "<digests> parallel digests, def: %d%s" default formula)
};

let poll_forever =
  Param.flag
    "poll-forever"
    aliases::["P"]
    Spec.no_arg
    doc::" poll filesystem for changes (keep polling forever)";

let stop_on_first_error =
  Param.flag
    "stop-on-first-error" aliases::["Q"] Spec.no_arg doc::" stop when first error encounterd";

let verbose =
  Param.flag
    "verbose"
    aliases::["--verbose"]
    Spec.no_arg
    doc::" Show full command string, and stdout/stderr from every command run";

let show_actions_run =
  Param.flag
    "show-actions-run"
    aliases::["act", "rr"]
    Spec.no_arg
    doc::" Show actions being run; and the reason why";

let show_actions_run_verbose =
  Param.flag
    "show-actions-run-verbose"
    aliases::["act-verbose", "rr-verbose"]
    Spec.no_arg
    doc::" Be more verbose about the reason actions are run";

let show_buildable_discovery =
  Param.flag
    "show-buildable-discovery"
    aliases::["buildable"]
    Spec.no_arg
    doc::" Mainly for debug. Shows discovery of buildable targets in a directory";

let show_checked =
  Param.flag
    "show-checked" aliases::["nr"] Spec.no_arg doc::" Show actions which are checked, but not run";

let show_considering =
  Param.flag
    "show-considering"
    aliases::["con"]
    Spec.no_arg
    doc::" Mainly for debug. Shows when deps are considered/re-considered (rather verbose)";

let show_error_dependency_paths =
  Param.flag
    "show-error-dependency-paths"
    Spec.no_arg
    doc::" show dependency paths from root goal to each error (like exception stack traces)";

let show_memory_allocations =
  Param.flag
    "show-memory-allocations"
    Spec.no_arg
    doc::" Show information about memory allocation at the end of the build";

let show_reconsidering =
  Param.flag
    "show-reconsidering"
    aliases::["recon"]
    Spec.no_arg
    doc::" Mainly for debug. Show when deps are re-considered";

let show_reflecting =
  Param.flag
    "show-reflecting"
    aliases::["reflect"]
    Spec.no_arg
    doc::" Mainly for debug. Shows when deps are being reflected";

let show_trace_messages =
  Param.flag "trace" Spec.no_arg doc::" switch on some additional trace messages";

let prefix_time = Param.flag "time" Spec.no_arg doc::" prefix all messages with the time";

let report_long_cycle_times =
  Param.flag
    "report-long-cycle-times"
    aliases::["long"]
    (Spec.optional Spec.int)
    doc::"<ms> (for development) pass to Scheduler.report_long_cycle_times";

let omake_server =
  Param.flag
    "w"
    aliases::["-omake-server"]
    Spec.no_arg
    doc::" Omake compatability; declare omake-server is caller";

let output_postpone =
  Param.flag "--output-postpone" Spec.no_arg doc::" Omake compatability; ignored";

let progress =
  Param.flag
    "progress"
    aliases::["--progress"]
    Spec.no_arg
    doc::" Show periodic progress report (omake style)";

let path_to_jenga_conf =
  Param.flag
    "-path-to-jenga-conf"
    (Spec.optional Spec.string)
    doc::(sprintf " Specify path to <jenga.conf>; The repo_root is taken to be CWD.");

let brief_error_summary =
  Param.flag
    "brief-error-summary"
    Spec.no_arg
    doc::" Don't repeat stdout/stderr from failing commands in error summary";

let no_server =
  Param.flag
    "no-server" Spec.no_arg doc::" Don't start jenga server (queries to the server won't work)";

let minor_heap_size = {
  let default = 50;
  Param.flag
    "minor-heap-size"
    (Spec.optional_with_default default Spec.int)
    doc::(sprintf "<Mb> (default = %d Mb)" default)
};

let major_heap_increment = {
  let default = 200;
  Param.flag
    "major-heap-increment"
    (Spec.optional_with_default default Spec.int)
    doc::(sprintf "<Mb> (default = %d Mb)" default)
};

let space_overhead = {
  let default = 100;
  Param.flag
    "space-overhead"
    (Spec.optional_with_default default Spec.int)
    doc::(sprintf "<percent> (default = %d)" default)
};

let no_notifiers =
  Param.flag
    "no-notifiers"
    aliases::["nono"]
    Spec.no_arg
    doc::" Disable filesystem notifiers (inotify); polling wont work";

let no_fs_triggers =
  Param.flag
    "no-fs-triggers"
    Spec.no_arg
    doc::" For testing, only valid without notifiers. Makes jenga more strict by failing instead of potentially recovering when the file system changes.";

let buildable_targets_fixpoint_max = {
  let default = 5;
  Param.flag
    "buildable-targets-fixpoint-max"
    (Spec.optional_with_default default Spec.int)
    doc::(sprintf "<iters> (default = %d); 0 means no limit" default)
};

let sandbox_actions =
  Param.flag
    "-sandbox-actions"
    Spec.no_arg
    doc::" Check dependencies are right by running actions in a part of the filesystem where only the declared dependencies are available";

let deprecated_camlp4 =
  Param.flag
    "-camlp4"
    Spec.no_arg
    doc::" Preprocess files listed in jenga.conf using camlp4 instead of ppx (deprecated)";

let anon_demands = Spec.anon (Spec.sequence ("DEMAND" %: Spec.string));

let create_config
    j_number::j_number
    f_number::f_number
    d_number::d_number
    poll_forever::poll_forever
    stop_on_first_error::stop_on_first_error
    verbose::verbose
    show_actions_run::show_actions_run
    show_actions_run_verbose::show_actions_run_verbose
    show_buildable_discovery::show_buildable_discovery
    show_checked::show_checked
    show_considering::show_considering
    show_error_dependency_paths::show_error_dependency_paths
    show_memory_allocations::show_memory_allocations
    show_reconsidering::show_reconsidering
    show_reflecting::show_reflecting
    show_trace_messages::show_trace_messages
    prefix_time::prefix_time
    report_long_cycle_times::report_long_cycle_times
    omake_server::omake_server
    output_postpone::_
    progress::progress
    path_to_jenga_conf::path_to_jenga_conf
    brief_error_summary::brief_error_summary
    no_server::no_server
    minor_heap_size::minor_heap_size
    major_heap_increment::major_heap_increment
    space_overhead::space_overhead
    no_notifiers::no_notifiers
    no_fs_triggers::no_fs_triggers
    buildable_targets_fixpoint_max::buildable_targets_fixpoint_max
    sandbox_actions::sandbox_actions
    deprecated_camlp4::deprecated_camlp4
    anon_demands::anon_demands => {
  Config.j_number: j_number,
  f_number,
  d_number,
  poll_forever,
  stop_on_first_error,
  verbose,
  show_memory_allocations,
  show_actions_run: show_actions_run || show_actions_run_verbose,
  show_actions_run_verbose,
  show_checked,
  show_considering,
  show_buildable_discovery,
  show_reflecting,
  show_reconsidering,
  show_trace_messages,
  show_error_dependency_paths,
  prefix_time,
  report_long_cycle_times:
    Option.map report_long_cycle_times f::(fun ms => Time.Span.create ms::ms ()),
  progress:
    if progress {
      if omake_server {
        Some `omake_style
      } else {
        Some `jem_style
      }
    } else {
      None
    },
  dont_emit_kill_line: String.(terminal_type == "dumb"),
  path_to_jenga_conf,
  brief_error_summary,
  no_server,
  no_notifiers,
  no_fs_triggers,
  buildable_targets_fixpoint_max,
  sandbox_actions,
  deprecated_camlp4,
  demands: anon_demands,
  gc: {Config.Gc.minor_heap_size: minor_heap_size, major_heap_increment, space_overhead}
};

open Command.Let_syntax;

let config_param: Command.Param.t Config.t =
  Command.Let_syntax.(
    [%map_open
      {
        let j_number = j_number
        and f_number = f_number
        and d_number = d_number
        and poll_forever = poll_forever
        and stop_on_first_error = stop_on_first_error
        and verbose = verbose
        and show_actions_run = show_actions_run
        and show_actions_run_verbose = show_actions_run_verbose
        and show_buildable_discovery = show_buildable_discovery
        and show_checked = show_checked
        and show_considering = show_considering
        and show_error_dependency_paths = show_error_dependency_paths
        and show_memory_allocations = show_memory_allocations
        and show_reconsidering = show_reconsidering
        and show_reflecting = show_reflecting
        and show_trace_messages = show_trace_messages
        and prefix_time = prefix_time
        and report_long_cycle_times = report_long_cycle_times
        and omake_server = omake_server
        and output_postpone: _ = output_postpone
        and progress = progress
        and path_to_jenga_conf = path_to_jenga_conf
        and brief_error_summary = brief_error_summary
        and no_server = no_server
        and minor_heap_size = minor_heap_size
        and major_heap_increment = major_heap_increment
        and space_overhead = space_overhead
        and no_notifiers = no_notifiers
        and no_fs_triggers = no_fs_triggers
        and buildable_targets_fixpoint_max = buildable_targets_fixpoint_max
        and sandbox_actions = sandbox_actions
        and deprecated_camlp4 = deprecated_camlp4
        and anon_demands = anon_demands;
        create_config
          j_number::j_number
          f_number::f_number
          d_number::d_number
          poll_forever::poll_forever
          stop_on_first_error::stop_on_first_error
          verbose::verbose
          show_actions_run::show_actions_run
          show_actions_run_verbose::show_actions_run_verbose
          show_buildable_discovery::show_buildable_discovery
          show_checked::show_checked
          show_considering::show_considering
          show_error_dependency_paths::show_error_dependency_paths
          show_memory_allocations::show_memory_allocations
          show_reconsidering::show_reconsidering
          show_reflecting::show_reflecting
          show_trace_messages::show_trace_messages
          prefix_time::prefix_time
          report_long_cycle_times::report_long_cycle_times
          omake_server::omake_server
          output_postpone::output_postpone
          progress::progress
          path_to_jenga_conf::path_to_jenga_conf
          brief_error_summary::brief_error_summary
          no_server::no_server
          minor_heap_size::minor_heap_size
          major_heap_increment::major_heap_increment
          space_overhead::space_overhead
          no_notifiers::no_notifiers
          no_fs_triggers::no_fs_triggers
          buildable_targets_fixpoint_max::buildable_targets_fixpoint_max
          sandbox_actions::sandbox_actions
          deprecated_camlp4::deprecated_camlp4
          anon_demands::anon_demands
      }
    ]
  );

let buildCommand toplevel::toplevel run::run () =>
  Command.basic'
    summary::("build specified targets" ^ (if toplevel {""} else {" (default subcommand)"}))
    readme::(
      fun () => {
        let rest =
          if toplevel {
            ["To see other jenga commands, try jenga help."]
          } else {
            []
          };
        String.concat sep::"\n" ["By default building the .DEFAULT target.", ...rest]
      }
    )
    (Command.Param.map config_param f::(fun config () => run config));

let main argv::argv=(Array.to_list Sys.argv) run::run () => {
  let toplevel_group = [("build", buildCommand toplevel::false run::run ())];
  let toplevel_group_names = ["help", "version", ...List.map toplevel_group f::fst];
  switch argv {
  | [_, s, ..._] when List.mem toplevel_group_names s =>
    Command.run (Command.group summary::"Build system for Reason" toplevel_group) argv::argv
  | _ =>
    /* When completing the first argument we would like to ask for the completion of
       both the group names and the flags/arguments of the command below. Unfortunately,
       Command wants to exit instead of returning even when completing. So we create the
       completion ourselves, which is easy enough, even though it's a bit ugly. */
    switch argv {
    | [_, s, ..._] when Sys.getenv "COMP_CWORD" == Some "1" =>
      List.iter
        toplevel_group_names
        f::(
          fun group_name =>
            if (String.is_prefix prefix::s group_name) {
              print_endline group_name
            }
        )
    | _ => ()
    };
    Command.run (buildCommand toplevel::true run::run ()) argv::argv
  }
};

let module Rel = Path.Rel;

let find_ancestor_directory_containing one_of::one_of => {
  if (List.is_empty one_of) {
    invalid_arg "find_ancestor_directory_containing"
  };
  let exists_in dir::dir => {
    let exists path =>
      switch (Core.Std.Sys.file_exists (dir ^\/ Rel.to_string path)) {
      | `No
      | `Unknown => false
      | `Yes => true
      };
    List.exists one_of f::exists
  };
  let start_dir = Core.Std.Sys.getcwd ();
  let rec loop dir =>
    if (exists_in dir::dir) {
      Ok (Path.Abs.create dir)
    } else if (String.equal dir Filename.root) {
      Or_error.errorf
        "Can't find %s in start-dir or any ancestor dir"
        (
          switch (List.rev one_of) {
          | [] => assert false
          | [x] => "Path.Rel Fails"
          | [x, ...l] => "Path.Rel Fails A lot"
          }
        )
    } else {
      loop (Filename.dirname dir)
    };
  loop start_dir
};

/*
  =====================================================
    COPIED CODE END
  =====================================================
 */
open Jenga_lib.Build.Jr_spec;

let root_markers = [".git", ".hg", "package.json"] |> List.map f::Path.Rel.create;

let () =
  main
    ()
    run::(
      fun config => {
        let root_dir = find_ancestor_directory_containing one_of::root_markers |> ok_exn;
        Run.main' root_dir::root_dir (Env Setup.env) config
      }
    );
