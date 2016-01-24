type t = {name: Symbol.t; parent: t option; frame: Frame.t; label: Temp.label; levelId: int}

type access = Frame.access * t

let nextLevel = ref 0

let mainLevel : t = {name: Symbol.of_string "_main", parent: None, frame: [], label: Temp.new_label ()}

let new_level name parent symEscapePairs =
	let frame = Frame.new_frame symEscapePairs in
	let label = Temp.new_label () in
	nextLevel := !nextLevel + 1
	{name; parent = Some parent; frame; label; levelId = !nextLevel - 1}

let alloc_local level escape : access =
	let access = Frame.alloc_local level.frame escape in
	(access, level)

let equal level1 level2 =
	level1.levelId = level2.levelId