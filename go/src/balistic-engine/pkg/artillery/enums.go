package artillery

type ProjectileStatus int
type LauncherStatus int

const VELOCITY_FACTOR = 0.985
const TIME_INTERVALS = 100
const TIME_FACTOR = 1.5
const INTERVALS = TIME_FACTOR * TIME_INTERVALS

const (
	Fired ProjectileStatus = iota + 1
	Success
	Failed
)

const (
	Unloaded LauncherStatus = iota + 1
	Loaded
	InUse
)

func (status ProjectileStatus) String() string {
	return [...]string{"Fired", "Success", "Failed"}[status-1]
}

func (s ProjectileStatus) EnumIndex() int {
	return int(s)
}

func (status LauncherStatus) String() string {
	return [...]string{"Unloaded", "Loaded", "InUse"}[status]
}

func (s LauncherStatus) EnumIndex() int {
	return int(s)
}
