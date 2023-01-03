package message

var messages = map[string]string

func init() {
	messages = map[string]string{
		"projectile.missed": "Projectile missed",
		"projectile.hit":    "Projectile hit",
	}
}