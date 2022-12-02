package artillery

import (
	"balistic-engine/pkg/config"
	"balistic-engine/pkg/math"
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

type Launcher struct {
	ID          string
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

func NewLauncher(coordinates math.Coordinates) *Launcher {
	return &Launcher{
		ID:          uuid.NewString(),
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

func (l *Launcher) AutoFire(interval_time_ms int) []*Proectile {
	if l.Configs == nil || len(l.Configs) == 0 {
		config.AppLogger.Error("No shooting configurations found")
		return nil
	}
	rr_counter := 0
	var projectiles []*Proectile = make([]*Proectile, l.AvailableAmmunitions())
	for {
		if l.IsReady() {
			index := rr_counter % len(l.Configs)
			projectile := l.Fire(uuid.Must(uuid.NewRandom()).String(), l.Configs[index].Velocity, l.Configs[index].Radians)
			projectiles = append(projectiles, projectile)
			rr_counter++
			time.Sleep(time.Duration(interval_time_ms) * time.Millisecond)
		} else {
			break
		}
	}
	return projectiles
}

func (l *Launcher) Fire(id string, velocity float64, radians float64) *Proectile {
	if l.IsReady() {
		start := time.Now()
		projectile := Fire(id, l.Coordinates, velocity, radians)
		l.fired++
		config.AppLogger.Info("*** Projectile fired", zap.String("Projectile ID: ", id),
			zap.Duration("Duration: ", time.Since(start)))
		l.Active = append(l.Active, projectile)
		return projectile
	}
	config.AppLogger.Error("The weapone is not ready to fire(unloaded)",
		zap.String("Launcher ID: ", l.ID),
		zap.Time("Last loaded time: ", l.loadedAt))
	return nil
}

func (l *Launcher) Destroy(id string) {
	var projectile *Proectile = nil
	for _, v := range l.Active {
		if v.ID == id {
			projectile = v
			break
		}
	}
	if projectile != nil {
		if projectile.status == Fired {
			projectile.Down()
		}
	} else {
		config.AppLogger.Error("Projectile not found", zap.String("Projectile ID: ", id))
	}
}
