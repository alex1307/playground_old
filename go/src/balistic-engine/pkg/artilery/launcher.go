package artilery

import (
	"balistic-engine/pkg/math"
	"time"

	"github.com/google/uuid"
)

type Launcher struct {
	ID          string
	Coordinates math.Coordinates
	Degrees     float64
	Capasity    int
	Missles     []Missle
	status      LauncherStatus
}

func NewLauncher(coordinates math.Coordinates) *Launcher {
	return &Launcher{
		ID:          uuid.Must(uuid.NewRandom()).String(),
		Coordinates: coordinates,
		status:      Unloaded,
	}
}

func (l *Launcher) GetStatus() LauncherStatus {
	return l.status
}

func (l *Launcher) Load(capacity int, missle Missle, degrees float64) {
	l.Capasity = capacity
	l.Degrees = degrees
	for i := 0; i < capacity; i++ {
		l.Missles = append(l.Missles, missle)
	}
	l.status = Loaded
}

func (l *Launcher) Fire(time_interval_ms int) []Proectile {
	if l.status == Loaded {
		l.status = InUse
		var projectiles []Proectile
		radians, _ := math.DegreesToRadians(l.Degrees)
		for _, m := range l.Missles {
			time.Sleep(time.Duration(time_interval_ms) * time.Millisecond)
			projectile := Fire(m.ID, l.Coordinates, m.Velocity, radians)
			projectiles = append(projectiles, *projectile)
		}
		l.Missles = []Missle{}
		l.Capasity = 0
		l.status = Unloaded
		return projectiles
	}
	return nil
}
