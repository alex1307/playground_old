package artillery

import (
	"balistic-engine/pkg/config"
	"balistic-engine/pkg/math"
	"balistic-engine/pkg/message"
	"math/rand"
	"time"

	"github.com/google/uuid"
	"go.uber.org/zap"
)

func NewLauncherConfiguration(coordinates math.Coordinates, capacity int, settings ShootingConfiguration, automat_shooting bool) LauncherConfiguration {
	return LauncherConfiguration{
		id:               uuid.NewString(),
		coordinates:      coordinates,
		capacity:         capacity,
		settings:         settings,
		automat_shooting: automat_shooting,
	}
}

func LaunchMissleSystem(name string, broker *message.Server, coordinates []math.Coordinates) *MissleSystem {
	lnumber := len(coordinates)
	launchers := make([]*Launcher, lnumber)
	for i, c := range coordinates {
		launchers[i] = NewLauncher(name, broker, c)
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
