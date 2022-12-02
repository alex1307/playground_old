package artillery

import (
	"balistic-engine/pkg/config"
	"balistic-engine/pkg/math"
	"math/rand"
	"time"

	"github.com/google/uuid"
	"go.uber.org/zap"
)

type MissleSystem struct {
	Name         string
	Launcher     []*Launcher
	Active       int
	Succeeded    int
	Failed       int
	trajectories chan TrajectoryCoordinates
}

type FireSettings struct {
	Radians  float64
	Velocity float64
}

type LauncherConfiguration struct {
	id               string
	coordinates      math.Coordinates
	capacity         int
	settings         FireSettings
	automat_shooting bool
}

func NewLauncherConfiguration(coordinates math.Coordinates, capacity int, settings FireSettings, automat_shooting bool) LauncherConfiguration {
	return LauncherConfiguration{
		id:               uuid.NewString(),
		coordinates:      coordinates,
		capacity:         capacity,
		settings:         settings,
		automat_shooting: automat_shooting,
	}
}

func NewFireSettings(radians, velocity float64) FireSettings {
	return FireSettings{
		Radians:  radians,
		Velocity: velocity,
	}
}

func LaunchMissleSystem(name string, coordinates []math.Coordinates) *MissleSystem {
	lnumber := len(coordinates)
	launchers := make([]*Launcher, lnumber)
	for i, c := range coordinates {
		launchers[i] = NewLauncher(c)
	}
	return &MissleSystem{
		Name:         name,
		Launcher:     launchers,
		trajectories: make(chan TrajectoryCoordinates, lnumber*MIN_CAPACITY)}
}

func (ms *MissleSystem) Load() {
	rand.Seed(time.Now().UnixNano())
	capacity := MIN_CAPACITY + int(rand.Intn(MAX_CAPACITY-MIN_CAPACITY))
	all := 0
	for _, l := range ms.Launcher {
		l.Load(capacity)
		all += capacity
	}
	config.AppLogger.Info("Missle system is loaded and ready to fire",
		zap.String("Artilery name: ", ms.Name),
		zap.Int("Number of weapons: ", len(ms.Launcher)),
		zap.Int("Number of ammunitions: ", all))
}

func (ms *MissleSystem) Fire(time_interval_ms int) {
	rand.Seed(time.Now().UnixNano())
	for _, l := range ms.Launcher {
		counter := int(l.AvailableAmmunitions())
		for i := 0; i < counter; i++ {
			radians, _ := math.DegreesToRadians(float64(MIN_DEGREE + rand.Intn(MAX_DEGREE-MIN_DEGREE)))
			velocity := float64(MIN_VELOCITY + rand.Intn(int(MAX_VELOCITY-MIN_VELOCITY)))
			uuid := uuid.NewString()
			projectile := l.Fire(uuid, velocity, radians)
			if projectile != nil {
				config.AppLogger.Info("Projectile fired", zap.String("Projectile ID: ", projectile.ID))
			}

			time.Sleep(time.Duration(time_interval_ms) * time.Millisecond)
		}
	}
}

func (ms *MissleSystem) Monitor() chan TrajectoryCoordinates {
	all := 0
	for _, l := range ms.Launcher {
		all += l.AvailableAmmunitions()
	}

	config.AppLogger.Info("Missle system is monitoring trajectories",
		zap.String("Artilery name: ", ms.Name),
		zap.Int("Number of weapons: ", len(ms.Launcher)),
		zap.Int("Number of ammunitions: ", all))

	var trajectory chan TrajectoryCoordinates = make(chan TrajectoryCoordinates, all*TIME_INTERVALS)
	for _, l := range ms.Launcher {
		go func(l *Launcher) {
			for _, p := range l.Active {
				go func(p *Proectile) {
					for coordinates := range p.Trajectory() {
						trajectory <- coordinates
					}
				}(p)
			}
		}(l)
	}
	return trajectory
}
