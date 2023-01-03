package config

var TEAM_RS string = "Russia"
var TEAM_UA string = "Ukraine"

var LAUNCHER_CHANNEL string = "launcher"
var MISSLE_CHANNEL string = "missle"
var PROJECTILE_CHANNEL string = "projectile"
var RADAR_CHANNEL string = "radar"
var TARGET_CHANNEL string = "target"

type LauncherStatus int
type ProjectileStatus int

const VELOCITY_FACTOR = 0.985
const TIME_INTERVALS = 100
const TIME_FACTOR = 1.5
const INTERVALS = TIME_FACTOR * TIME_INTERVALS

const (
	Fired ProjectileStatus = iota + 1
	Success
	TakenDown
	Missed
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

func TargetTeamID(sourceTeamID string) string {
	if sourceTeamID == TEAM_UA {
		return TEAM_RS
	}
	return sourceTeamID
}

func TargetChannelName(teamId string) string {
	return TARGET_CHANNEL + teamId
}
