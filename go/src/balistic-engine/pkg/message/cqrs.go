package message

type Topic struct {
	Name        string
	Threshold   int
	messages    []byte
	subscribers []string
}
