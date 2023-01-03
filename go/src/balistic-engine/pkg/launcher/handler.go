package launcher

import (
	"balistic-engine/pkg/config"
	"balistic-engine/pkg/math"
	"balistic-engine/pkg/message"
	"time"

	"github.com/google/uuid"
	"go.uber.org/zap"
)

var MIN_DEGREE = 30
var MAX_DEGREE = 75

var MIN_VELOCITY = 80
var MAX_VELOCITY = 200

var MIN_CAPACITY = 10
var MAX_CAPACITY = 50

func NewLauncher(teamId string, broker *message.Server, coordinates math.Coordinates) *Launcher {
	source := make(chan message.Payload, 100)
	broker.Subscribe(teamId, source)
	return &Launcher{
		TeamID:      teamId,
		ID:          uuid.NewString(),
		broker:      broker,
		source:      source,
		Coordinates: coordinates,
		Configs:     nil,
		loaded:      0,
		fired:       0,
		loadedAt:    time.Time{},
	}
}

func (l *Launcher) Load(live_ammuntions int) {
	l.loaded = l.AvailableAmmunitions() + live_ammuntions
	l.fired = 0
	l.loadedAt = time.Now()
}

func (l *Launcher) ConfigureShooting(config []ShootingConfiguration) {
	l.Configs = config
}

func (l *Launcher) AvailableAmmunitions() int {
	return l.loaded - l.fired
}

func (l *Launcher) IsReady() bool {
	return l.loaded-l.fired > 0
}

func (l *Launcher) AutoFire(interval_time_ms int) {
	if l.Configs == nil || len(l.Configs) == 0 {
		config.AppLogger.Error("No shooting configurations found")
		return
	}
	rr_counter := 0

	for {
		if l.IsReady() {
			index := rr_counter % len(l.Configs)
			id := uuid.NewString()
			l.Fire(id, l.Configs[index].Velocity, l.Configs[index].Radians)
			rr_counter++
			time.Sleep(time.Duration(interval_time_ms) * time.Millisecond)
			config.AppLogger.Info("Fired",
				zap.Int("Fired: ", l.fired),
				zap.String("uuid: ", id),
				zap.String("team: ", l.TeamID),
			)
		} else {
			break
		}
	}
}

func (l *Launcher) Fire(id string, velocity float64, radians float64) {
	if l.IsReady() {
		// start := time.Now()
		message := message.NewMessage(l.TeamID, []string{"missle"}, &Missle{
			ID:          id,
			TeamID:      l.TeamID,
			Coordinates: l.Coordinates,
			Velocity:    velocity,
			Radians:     radians})
		l.broker.Send(message)
		l.fired++
		// config.AppLogger.Info("*** Projectile fired", zap.String("Projectile ID: ", id),
		// 	zap.Duration("Duration: ", time.Since(start)))

	} else {
		config.AppLogger.Error("The weapone is not ready to fire(unloaded)",
			zap.String("Launcher ID: ", l.ID),
			zap.Time("Last loaded time: ", l.loadedAt))
	}

}

func (m *Missle) GetID() string {
	return m.ID
}

func (m *Missle) GetTeamID() string {
	return m.TeamID
}

func (m *Missle) GetCoordinates() math.Coordinates {
	return m.Coordinates
}

func (m *Missle) GetVelocity() float64 {
	return m.Velocity
}
