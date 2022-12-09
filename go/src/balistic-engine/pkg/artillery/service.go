package artillery

import (
	config "balistic-engine/pkg/config"

	"github.com/google/uuid"
	"go.uber.org/zap"
)

// var Artillery map[string]*MissleSystem

type Artillery interface {
	AddLauncherToMissleSystem(launcher *Launcher) *MissleSystem
	LoadLauncher(launcher_id string, live_ammunitions int, config []ShootingConfiguration)
	Filter(launcher_id string) []*Launcher
	AutomaticFire(launcher_id string, interval_time_ms int)
	ManualFire(launcher_id string, velocity float64, radians float64)
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

func (ms *MissleSystem) AutomaticFire(launcher_id string, interval_time_ms int) {

	for _, launcher := range ms.Launcher {
		if launcher_id == "" || launcher_id == "*" || launcher.ID == launcher_id {
			launcher.AutoFire(interval_time_ms)
		}
	}
}

func (ms *MissleSystem) ManualFire(launcher_id string, velocity float64, radians float64) {

	for _, launcher := range ms.Launcher {
		if launcher.ID == launcher_id {
			launcher.Fire(uuid.NewString(), velocity, radians)
		}
	}
	config.AppLogger.Error("Launcher not found", zap.String("launcher_id", launcher_id))

}
