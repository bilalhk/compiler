type t = {name: Symbol.t; parent: t; frame: Frame.t; label: Temp.label}

type access = Frame.access * t

let new_level name parent symEscapePairs =
	let frame = Frame.new_frame symEscapePairs in
	let label = Temp.new_label () in
	{name; parent; frame; label}

let alloc_local level escape : access =
	let access = Frame.alloc_local level.frame escape in
	(access, level)