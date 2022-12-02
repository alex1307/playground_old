package artillery

import (
	config "balistic-engine/pkg/config"
	"context"
	"time"

	"github.com/google/uuid"
	"go.uber.org/zap"
)

// var Artillery map[string]*MissleSystem

type Artillery interface {
	AddLauncherToMissleSystem(launcher *Launcher) *MissleSystem
	LoadLauncher(launcher_id string, live_ammunitions int, config []ShootingConfiguration)
	Filter(launcher_id string) []*Launcher
	AutomaticFire(launcher_id string, interval_time_ms int) []*Proectile
	ManualFire(launcher_id string, velocity float64, radians float64) *Proectile
	Monitoring(ctx context.Context) chan TrajectoryCoordinates
}

func NewMissleSystem(name string) *MissleSystem {
	return &MissleSystem{
		Name:     name,
		Launcher: nil,
	}
}

func (ms *MissleSystem) AddLauncherToMissleSystem(launcher *Launcher) *MissleSystem {
	ms.Launcher = append(ms.Launcher, launcher)
	return ms
}

func (ms *MissleSystem) Filter(launcher_id string) []*Launcher {

	if launcher_id == "" || launcher_id == "*" {
		return ms.Launcher
	}

	for _, launcher := range ms.Launcher {
		if launcher.ID == launcher_id {
			return []*Launcher{launcher}
		}
	}
	config.AppLogger.Error("Launcher not found", zap.String("launcher_id", launcher_id))
	return nil
}

func (ms *MissleSystem) LoadLauncher(launcher_id string, live_ammunitions int, configuration []ShootingConfiguration) {

	for _, launcher := range ms.Launcher {
		if launcher_id == "" || launcher.ID == launcher_id {
			launcher.ConfigureShooting(configuration)
			launcher.Load(live_ammunitions)
		}
	}
}

func (ms *MissleSystem) AutomaticFire(launcher_id string, interval_time_ms int) []*Proectile {

	var projectiles []*Proectile = nil
	for _, launcher := range ms.Launcher {
		if launcher_id == "" || launcher_id == "*" || launcher.ID == launcher_id {
			launcher.AutoFire(interval_time_ms)
			for _, projectile := range projectiles {
				if projectile != nil {
					config.AppLogger.Info(" --> projectile", zap.String("projectile_id", projectile.ID))
				}

			}
		}
	}
	return projectiles
}

func (ms *MissleSystem) ManualFire(launcher_id string, velocity float64, radians float64) *Proectile {

	for _, launcher := range ms.Launcher {
		if launcher.ID == launcher_id {
			projectile := launcher.Fire(uuid.NewString(), velocity, radians)
			return projectile
		}
	}
	config.AppLogger.Error("Launcher not found", zap.String("launcher_id", launcher_id))
	return nil
}

func (ms *MissleSystem) isShootingStarted(ctx context.Context) bool {
	if ms.Launcher == nil || len(ms.Launcher) == 0 {
		return false
	}

	for _, launcher := range ms.Launcher {
		if launcher.Active != nil && len(launcher.Active) > 0 {
			return true
		}
	}

	return false
}

func (ms *MissleSystem) Monitoring(ctx context.Context) chan TrajectoryCoordinates {
	all := 0
	for _, l := range ms.Launcher {
		all += l.AvailableAmmunitions()
	}
	var trajectory chan TrajectoryCoordinates = make(chan TrajectoryCoordinates, all*TIME_INTERVALS)
	config.AppLogger.Info("Missle system is monitoring trajectories",
		zap.String("Artilery name: ", ms.Name),
		zap.Int("Number of weapons: ", len(ms.Launcher)),
		zap.Int("Number of ammunitions: ", all))
	go func() {
		counter := 0
		for {
			if ms.isShootingStarted(ctx) {
				break
			}

			if counter++; counter%5 == 0 {
				config.AppLogger.Info("Waiting for shooting to start")
			}
			time.Sleep(1 * time.Second)
		}

		for _, l := range ms.Launcher {
			config.AppLogger.Info("##### -> Tracing projectile", zap.String("launcher_id", l.ID))
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
	}()

	return trajectory
}
