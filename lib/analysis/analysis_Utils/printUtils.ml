module Parser = struct
  type atomic_list = DataStructures.Parser.Commands.atomic_t list [@@deriving show]
end

module Analysis = struct
  type commands_list = DataStructures.Analysis.Commands.t list [@@deriving show]
end