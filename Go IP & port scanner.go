package main

import (
	"flag"
	"fmt"
	"net"
	"strconv"
	"strings"
	"sync"
	"time"
)

type Scanner struct {
	IP      string
	Ports   string
	Timeout int
}

const (
	colorRed   = "\033[31m"
	colorGreen = "\033[32m"
	colorBlue  = "\033[34m"
	colorWhite = "\033[37m"
)

var wg sync.WaitGroup

// Parses command-line arguments
func (s *Scanner) Setup() {
	flag.StringVar(&s.IP, "i", "", "IP address or domain")
	flag.StringVar(&s.Ports, "p", "", "Ports separated by commas or ranges (e.g., 22,80,443 or 20-30)")
	flag.IntVar(&s.Timeout, "t", 200, "Set the timeout in milliseconds")
	flag.Parse()
}

func ipRange(startIP, endIP string) ([]string, error) {
	start := net.ParseIP(startIP).To4()
	end := net.ParseIP(endIP).To4()
	if start == nil || end == nil {
		return nil, fmt.Errorf("invalid IP range")
	}

	var ips []string
	for ip := start; !ip.Equal(end); incrementIP(ip) {
		ips = append(ips, ip.String())
	}
	ips = append(ips, end.String()) // Include the last IP
	return ips, nil
}

func incrementIP(ip net.IP) {
	for i := len(ip) - 1; i >= 0; i-- {
		ip[i]++
		if ip[i] > 0 {
			break
		}
	}
}

func parsePorts(portInput string) ([]int, error) {
	var ports []int
	if strings.Contains(portInput, "-") {
		rangeParts := strings.Split(portInput, "-")
		if len(rangeParts) != 2 {
			return nil, fmt.Errorf("invalid port range")
		}
		start, err1 := strconv.Atoi(rangeParts[0])
		end, err2 := strconv.Atoi(rangeParts[1])
		if err1 != nil || err2 != nil || start > end {
			return nil, fmt.Errorf("invalid port range")
		}
		for i := start; i <= end; i++ {
			ports = append(ports, i)
		}
	} else {
		for _, p := range strings.Split(portInput, ",") {
			port, err := strconv.Atoi(p)
			if err != nil {
				return nil, fmt.Errorf("invalid port: %s", p)
			}
			ports = append(ports, port)
		}
	}
	return ports, nil
}

func scanPortTCP(ip string, port, timeout int) {
	defer wg.Done()
	address := fmt.Sprintf("%s:%d", ip, port)
	conn, err := net.DialTimeout("tcp", address, time.Millisecond*time.Duration(timeout))
	if err == nil {
		defer conn.Close()
		fmt.Printf("%d %s[Open]%s\n", port, colorGreen, colorWhite)
	} else {
		fmt.Printf("%d %s[Closed]%s\n", port, colorRed, colorWhite)
	}
}

func main() {
	scanner := &Scanner{}
	scanner.Setup()

	if scanner.IP == "" {
		fmt.Println("Set an IP address with -i")
		return
	}

	// Handle ports
	ports, err := parsePorts(scanner.Ports)
	if err != nil {
		fmt.Printf("%sError: %s%s\n", colorRed, err.Error(), colorWhite)
		return
	}

	// Handle IPs
	var ips []string
	if strings.Contains(scanner.IP, "-") {
		ipRangeParts := strings.Split(scanner.IP, "-")
		if len(ipRangeParts) != 2 {
			fmt.Printf("%sInvalid IP range%s\n", colorRed, colorWhite)
			return
		}
		ips, err = ipRange(ipRangeParts[0], ipRangeParts[1])
		if err != nil {
			fmt.Printf("%sError: %s%s\n", colorRed, err.Error(), colorWhite)
			return
		}
	} else {
		ips = []string{scanner.IP}
	}

	// Scan ports for each IP
	fmt.Printf("%sScanning IPs and Ports%s\n", colorBlue, colorWhite)
	for _, ip := range ips {
		fmt.Printf("Scanning IP: %s\n", ip)
		for _, port := range ports {
			wg.Add(1)
			go scanPortTCP(ip, port, scanner.Timeout)
		}
	}
	wg.Wait()
}
