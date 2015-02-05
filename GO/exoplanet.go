package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"bufio"
	"container/list"
)

type Planet struct {
	Primaryidentifierofplanet string;
	Binaryflag float64;
	Planetarymass float64;
	Radius float64;
	Period float64;
	Semimajoraxis float64;
	Eccentricity float64;
	Periastron float64;
	Longitude float64;
	Ascendingnode float64;
	Inclination float64;
	Surfaceorequilibriumtemperature float64;
	Age float64;
	Discoverymethod string;
	Discoveryyear float64;
	Lastupdated float64;
	Rightascension float64;
	Declination float64;
	Distancefromsun float64;
	Hoststarmass float64;
	Hoststarradius float64;
	Hoststarmetallicity float64;
	Hoststartemperature float64;
	Hoststarage float64;
}

func newplanet(fields []string) Planet {
	var Primaryidentifierofplanet string
	var Discoverymethod string
	
	Primaryidentifierofplanet = fields[0]
	Binaryflag , _ := strconv.ParseFloat(fields[1], 64);
    Planetarymass , _ := strconv.ParseFloat(fields[2], 64)
    Radius , _ := strconv.ParseFloat(fields[3], 64)
    Period , _ := strconv.ParseFloat(fields[4], 64)
    Semimajoraxis , _ := strconv.ParseFloat(fields[5], 64)
    Eccentricity , _ := strconv.ParseFloat(fields[6], 64)
    Periastron , _ := strconv.ParseFloat(fields[7], 64)
    Longitude , _ := strconv.ParseFloat(fields[8], 64)
    Ascendingnode , _ := strconv.ParseFloat(fields[9], 64)
    Inclination , _ := strconv.ParseFloat(fields[10], 64)
    Surfaceorequilibriumtemperature , _ := strconv.ParseFloat(fields[11], 64)
    Age , _ := strconv.ParseFloat(fields[12], 64)
	Discoverymethod = fields[13]
    Discoveryyear , _ := strconv.ParseFloat(fields[14], 64)
    Lastupdated , _ := strconv.ParseFloat(fields[15], 64)
    Rightascension , _ := strconv.ParseFloat(fields[16], 64)
    Declination , _ := strconv.ParseFloat(fields[17], 64)
    Distancefromsun , _ := strconv.ParseFloat(fields[18], 64)
    Hoststarmass , _ := strconv.ParseFloat(fields[19], 64)
    Hoststarradius , _ := strconv.ParseFloat(fields[20], 64)
    Hoststarmetallicity , _ := strconv.ParseFloat(fields[21], 64)
    Hoststartemperature , _ := strconv.ParseFloat(fields[22], 64)
    Hoststarage , _ := strconv.ParseFloat(fields[23], 64)
	
	return Planet{Primaryidentifierofplanet, Binaryflag, Planetarymass, Radius, Period, Semimajoraxis, Eccentricity, Periastron, Longitude,
		Ascendingnode, Inclination, Surfaceorequilibriumtemperature, Age, Discoverymethod, Discoveryyear, Lastupdated, Rightascension,
		Declination, Distancefromsun, Hoststarmass, Hoststarradius, Hoststarmetallicity, Hoststartemperature, Hoststarage}
}

//reads the file and creates planets
func readfile(name string) (planets []Planet) {

	//open ifle
	filein, error:= os.Open(name)
	if error != nil {
		panic(error)
	}
	
	defer func(){
		if error:=filein.Close(); error !=nil{
		panic(error)
		}
	}()
	scanner := bufio.NewScanner(filein)
	
	//throw away first 30 lines
	for i := 0; i < 30; i++ {
		scanner.Scan()
	}
	
	//read the rest
	for scanner.Scan() {
		if fields := strings.Split(scanner.Text(), ","); len(fields) == 24 {
			planets = append(planets, newplanet(fields))
		}
	}
	return;
}

//counts the number of binary planets
func countbinaryplanets(planets []Planet) (counter int) {
	for _, p := range planets {
		if p.Binaryflag ==2 {
				counter++
		}
	}
	return;
}

//counts the number of plantary systems
func countplantarysystems(planets []Planet) (count int) {
	var pid string
	var l list.List
	
	for _, p:= range planets {
		var found bool = false
		
		if p.Primaryidentifierofplanet != "" {
			pid=p.Primaryidentifierofplanet[:len(p.Primaryidentifierofplanet)-1]
			
			//very inefficient will fix if I have time 
			for e := l.Front(); e != nil; e=e.Next() {
				if e.Value == pid {
					found=true
				}
			}
			if found != true{
				l.PushBack(pid)	
			}
		}
	}
	count = l.Len()
	return;	
}

//counts the number of confirmed planets
func countconfirmed(planets []Planet) (count int) {
	for _, p:= range planets {
		if p.Discoverymethod!= "" {
			count++
		}
	}
	return;
}

func main(){
	planets := readfile("open_exoplanet_catalogue.txt")
	
	fmt.Printf("There are %v total Planets (including Solar System objects and unconfirmed exoplanets).\n", len(planets))
	fmt.Printf("There are %v Binary systems.\n", countbinaryplanets(planets))
	fmt.Printf("There are %v Plantary systems.\n", countplantarysystems(planets))
	fmt.Printf("There are %v Confirmed exoplanets.\n", countconfirmed(planets))
}