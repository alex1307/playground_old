package math

import (
	"fmt"
	"math"

	"balistic-engine/model"
)

const G_FORCE = 9.80665

func Position(Velocity,
	Radians,
	Heigth,
	Time float64) (model.Coordinates, error) {
	//x = cos(Θ)•V•t
	//y = h + sin(Θ)•V•t - ½gt²

	cos_angel := math.Cos(Radians)
	sin_angle := math.Sin(Radians)
	x := cos_angel * Velocity * Time
	y := (Heigth + (sin_angle * Velocity * Time)) - 0.5*G_FORCE*math.Pow(Time, 2.0)
	return model.Coordinates{X: x, Y: y, T: Time}, nil
}

// Calculate elapsed projection time
// (V *sin(θ) + SQRT(V * sin(θ) + 2 * G * h))/G
func ElapsedTime(velocity, radians, height float64) (float64, error) {

	if velocity <= 0 {
		return 0.0, fmt.Errorf("invalid velocity param! Velocity must be a positive value m/s")
	}

	if height < 0 {
		return 0.0, fmt.Errorf("invalid parameter! The height is a positive number in meters")
	}

	v_sin := velocity * math.Sin(radians)
	attr := 2 * G_FORCE * height
	result := math.Pow(v_sin, 2.0) + attr
	Time := (v_sin + math.Sqrt(result)) / G_FORCE
	return Time, nil
}

func MaxRange(Velocity, Radians, Height float64) (float64, error) {
	ElapsedTime, err := ElapsedTime(Velocity, Radians, Height)
	if err != nil {
		return -1.0, err
	}

	return ElapsedTime * Velocity * math.Cos(Radians), nil

}

// Max = h + ((V * V * sin(θ) * sin(θ))/2 * G)
func MaxAltitude(velocity, degrees, height float64) (float64, error) {
	if velocity <= 0 {
		return 0.0, fmt.Errorf("invalid velocity param! The velocity must be a positive value m/s")
	}
	radians, err := DegreesToRadians(degrees)
	if err != nil {
		return 0.0, err
	}
	if height < 0 {
		return 0.0, fmt.Errorf("invalid parameter! The height is a positive number in meters")
	}
	Velocity_Y := velocity * math.Sin(radians)
	Velocity_2 := math.Pow(Velocity_Y, 2)
	Height_2 := 2 * G_FORCE
	return height + (Velocity_2 / Height_2), nil

}

func Velocity(Velocity, Radians, Height, Time float64) (model.Velocity, error) {
	if Velocity <= 0 {
		return model.Velocity{}, fmt.Errorf("invalid velocity param! The velocity must be a positive value m/s")
	}

	ElapsedTime, err := ElapsedTime(Velocity, Radians, Height)

	if err != nil {
		return model.Velocity{}, err
	}

	if Time >= ElapsedTime {
		return model.Velocity{}, nil
	}

	Velocity_X := Velocity * math.Cos(Radians)
	Velocity_Y := Velocity*math.Sin(Radians) - G_FORCE*Time

	return model.Velocity{X: Velocity_X, Y: Velocity_Y}, nil
}

func DegreesToRadians(degree float64) (float64, error) {

	if degree < 0 || degree > 90 {
		return 0.0, fmt.Errorf("invalid parameter! Valid degree is in range (0, 90)")
	}

	return (degree * math.Pi) / 180, nil

}

func TimeInterval(elapsed_time float64, seed int) ([]float64, error) {
	if elapsed_time <= 0 {
		return nil, fmt.Errorf("")
	}
	result := make([]float64, seed+1)
	for i := 0; i < seed; i++ {
		result[i] = float64(i) * elapsed_time / float64(seed)
	}
	result[seed] = elapsed_time
	return result, nil
}

func CalculateTrajection(velocity, degrees, height float64, seed int) ([]model.Coordinates, error) {
	radians, err := DegreesToRadians(degrees)
	if err != nil {
		return nil, err
	}

	max_time, err := ElapsedTime(velocity, radians, height)
	if err != nil {
		return nil, err
	}

	intervals, err := TimeInterval(max_time, seed)
	if err != nil {
		return nil, err
	}
	trajection := make([]model.Coordinates, len(intervals))
	for i, time := range intervals {
		position, errp := Position(velocity, radians, height, time)

		if errp != nil {
			return nil, errp
		}

		trajection[i] = position
	}
	return trajection, nil
}

func PositionToIndex(position, grid_initial_position model.Coordinates, grid_width, grid_height float64, grid_scale int) (int, error) {
	//grid := field.Grid
	length := position.X - grid_initial_position.X
	height := position.Y - grid_initial_position.Y

	if length < 0 || height < 0 || length > grid_width || height > grid_height {
		return -1, fmt.Errorf("coordinates are out of range")
	}

	step_x := grid_width / float64(grid_scale)
	step_y := grid_height / float64(grid_scale)
	x_index := int(length / step_x)
	y_index := int(height / step_y)

	return x_index*grid_scale + y_index, nil
}

// func CalculateTrajectionToGrid(velocity, degrees, height float64, seed int, grid model.Grid) ([]int, error) {
// 	trajection, err := CalculateTrajection(velocity, degrees, height, seed)
// 	if err != nil {
// 		return nil, err
// 	}
// 	result := make([]int, len(trajection))
// 	for i, position := range trajection {
// 		index, err := PositionToIndex(position, grid.InitialPosition, grid.Width, grid.Height, grid.Scale)
// 		if err != nil {
// 			return nil, err
// 		}
// 		result[i] = index
// 	}
// 	return result, nil
// }

// func CalculateTrajectionToGrid(velocity, degrees, height float64, seed int, grid model.Grid) ([]int, error) {
// 	trajection, err := CalculateTrajection(velocity, degrees, height, seed)
// 	if err != nil {
// 		return nil, err
// 	}
// 	result := make([]int, len(trajection))
// 	for i, position := range trajection {
// 		index, err := PositionToIndex(position, grid.InitialPosition, grid.Width, grid.Height, grid.Scale)
// 		if err != nil {
// 			return nil, err
// 		}
// 		result[i] = index
// 	}
// 	return result, nil
// }
