package projectile

import (
	"balistic-engine/pkg/config"
	"balistic-engine/pkg/math"
	"balistic-engine/pkg/message"
	"context"
	"sync"
	"time"
)

type TrajectoryCoordinates struct {
	ID          string
	TeamID      string
	SystemTime  int64
	Coordinates math.Coordinates
}

type ProjectileHandler struct {
	ID          string
	broker      *message.Server
	done        chan message.Payload
	missles     chan message.Payload
	projectiles map[string]ProjectileInfo
	wg          *sync.WaitGroup
}

type ProjectileInfo struct {
	ID         string
	Status     config.ProjectileStatus
	TimeToLive float64
	MaxRange   float64
	FireAt     time.Time
	cancel     context.CancelFunc
}
