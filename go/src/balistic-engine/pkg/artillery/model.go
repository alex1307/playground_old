package artillery

import (
	"balistic-engine/pkg/math"
	"balistic-engine/pkg/message"
	"time"
)

type Launcher struct {
	ID          string
	TeamID      string
	broker      *message.Server
	source      chan message.Payload
	Coordinates math.Coordinates
	Configs     []ShootingConfiguration
	loaded      int
	fired       int
	loadedAt    time.Time
}

type ShootingConfiguration struct {
	Velocity float64
	Radians  float64
}

type Missle struct {
	ID          string
	TeamID      string
	Coordinates math.Coordinates
	Velocity    float64
	Radians     float64
}

type MissleSystem struct {
	Name         string
	Launcher     []*Launcher
	Active       int
	Succeeded    int
	Failed       int
	trajectories chan TrajectoryCoordinates
}

type LauncherConfiguration struct {
	id               string
	coordinates      math.Coordinates
	capacity         int
	settings         ShootingConfiguration
	automat_shooting bool
}

type TrajectoryCoordinates struct {
	ID          string
	TeamID      string
	SystemTime  int64
	Coordinates math.Coordinates
}

type Radar interface {
	Subscribe(id string)
	GetSubscriptions() []string
	GetActiveIDs() []string
	IsActive() bool
	TraceProjectile(id string, missle Missle)
	ProjectileMonitor() chan TrajectoryCoordinates
}
