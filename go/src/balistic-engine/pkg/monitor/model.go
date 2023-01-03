package monitor

import (
	"balistic-engine/pkg/math"
	"balistic-engine/pkg/message"
	"balistic-engine/pkg/projectile"
)

type TakenDown struct {
	ID []string
}

type Space struct {
	seed        float64
	lenght_seed float64
	height_seed float64
	corners     [2]math.Coordinates
	projectiles map[string]projectile.TrajectoryCoordinates
	statisctic  map[string]projectile.ProjectileInfo
	active      map[string]int
}

type Radar struct {
	source chan message.Payload
	space  Space
	broker *message.Server
}
