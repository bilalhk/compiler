module F = Frame

type t = {name: Symbol.t; parent: t option; frame: F.t; label: Temp.label; levelId: int}

type access = F.access * t

let mainLevel = {name = Symbol.of_string "_main"; parent = None; frame = F.new_frame []; label = Temp.new_label (); levelId = 0}

let nextLevel = ref 1

let new_level name parent symEscapePairs =
	let frame = F.new_frame symEscapePairs in
	let label = Temp.new_label () in
	nextLevel := !nextLevel + 1;
	{name; parent = Some parent; frame; label; levelId = !nextLevel - 1}

let alloc_local level escape : access =
	let access = F.alloc_local level.frame escape in
	(access, level)

let equal level1 level2 =
	level1.levelId = level2.levelId