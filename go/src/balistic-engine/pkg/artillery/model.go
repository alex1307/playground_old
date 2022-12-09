package artillery

import (
	"balistic-engine/pkg/math"
	"balistic-engine/pkg/message"
	"time"

	"github.com/google/uuid"
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
	Active      []*Proectile
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

type Proectile struct {
	ID        string
	broker    *message.Server
	done      chan message.Payload
	missles   chan message.Payload
	status    ProjectileStatus
	ttl       float64
	max_range float64
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

type MessageImpl struct {
	id      string
	from    string
	to      []string
	content message.Payload
}

func NewMessage(from string, to []string, content message.Payload) *MessageImpl {
	return &MessageImpl{
		id:      uuid.NewString(),
		from:    from,
		to:      to,
		content: content,
	}
}

func (m *MessageImpl) GetID() string {
	return m.id
}

func (m *MessageImpl) From() string {
	return m.from
}

func (m *MessageImpl) To() []string {
	return m.to
}

func (m *MessageImpl) Content() message.Payload {
	return m.content
}
