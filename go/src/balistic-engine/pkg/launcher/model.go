package launcher

import (
	"balistic-engine/pkg/math"
	"balistic-engine/pkg/message"
	"time"
)

type ShootingConfiguration struct {
	Velocity float64
	Radians  float64
}

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

type Missle struct {
	ID          string
	TeamID      string
	Coordinates math.Coordinates
	Velocity    float64
	Radians     float64
}
