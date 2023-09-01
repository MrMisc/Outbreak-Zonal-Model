use std::thread;
use rand::distributions::Uniform;
use rand::distributions::{Distribution, Standard};
use rand::{thread_rng, Rng};
use statrs::distribution::{Normal, Poisson, StudentsT, Triangular, Weibull};

extern crate serde;
extern crate serde_json;
use serde::Deserializer;
use serde::{Deserialize, Serialize};
use serde_json::json;

// use std::error::Error;
use std::fs::File;
use std::fs::OpenOptions;
use std::io::Read;
use std::io::Write;
use std::time::{Duration, Instant};
use std::{fs, io, process};
use std::error::Error;

use csv::Writer;


pub mod limits{
    pub fn min(a:f64,b:f64)->f64{
        if a<b{
            a
        }
        else{
            b
        }
    }
    pub fn max(a:f64,b:f64)->f64{
        if a<b{
            b
        }
        else{
            a
        }
    }
}

pub fn normal(mean: f64, std: f64, upper:f64) -> f64 {
    let mut thing: f64 = 0.0;
    loop {
        let mut rng = thread_rng();
        let v: &Vec<f64> = &Normal::new(mean, std)
            .unwrap()
            .sample_iter(&mut rng)
            .take(1)
            .collect();
        thing = v[0];
        if thing > 0.0 && thing<upper{
            break;
        }
    }
    thing
}
#[derive(Clone)]
pub struct Zone{
    segments:Vec<Segment>, 
    zone:usize,
    capacity:u32
}

#[derive(Clone)]
pub struct Segment{
    zone:usize,
    origin_x:u64,
    origin_y:u64,
    range_x:u64,
    range_y:u64,
    capacity:u32
}

impl Zone{
    fn add(&mut self)->[u64;4]{
        println!("Adding to {}",self.zone);
        self.capacity-=1;
        let mut origin_x:u64 = 0;
        let mut origin_y:u64 = 0;
        let mut range_x:u64 = 0;
        let mut range_y:u64 = 0;
        if let Some(first_space) = self.segments.iter_mut().find(|item| item.capacity >= 1){ //0 occupants 
            //Segment capacity update
            first_space.capacity -= 1; //add 1 occupant
            origin_x = first_space.origin_x;
            origin_y = first_space.origin_y;
            range_x = first_space.range_x;
            range_y = first_space.range_y;
        }        
        [origin_x,origin_y,range_x,range_y]
    }
    fn subtract(mut self,x:u64,y:u64)->Zone{
        self.capacity+=1;
        self.segments = self.segments.into_iter().filter_map(|mut seg| {
            if x == seg.origin_x && y == seg.origin_y{
                seg.capacity += 1;
            }
            Some(seg)
        }).collect();
        self
    }
    fn generate_empty(zone:usize,grid:[u64;2],step:usize)->Zone{
        let mut vector:Vec<Segment> = Vec::new();
        for x in (0..grid[0]).step_by(step){
            for y in (0..grid[1]).step_by(step){
                vector.push(Segment{zone:zone.clone(),origin_x:x,origin_y:y,range_x:step.clone() as u64,range_y:step.clone() as u64,capacity:NO_OF_CHICKENS_PER_SEGMENT[zone] as u32})
            }
        }
        Zone{segments:vector,zone:zone, capacity:(grid[0] as u32)*(grid[1] as u32)/ ((step*step) as u32)*NO_OF_CHICKENS_PER_SEGMENT[zone] as u32}
    }
    fn generate_full(zone:usize,grid:[u64;2],step:usize)->Zone{
        let mut vector:Vec<Segment> = Vec::new();
        for x in (0..grid[0]).step_by(step){
            for y in (0..grid[1]).step_by(step){
                vector.push(Segment{zone:zone.clone(),origin_x:x,origin_y:y,range_x:step.clone() as u64,range_y:step.clone() as u64,capacity:0})
            }
        }
        Zone{segments:vector,zone:zone, capacity:0}
    }    
}

#[derive(Clone)]
pub struct host{
    infected:bool,
    motile:u8,
    zone:usize, //Possible zones denoted by ordinal number sequence
    prob1:f64,  //Probability of contracting disease - these are tied to zone if you create using .new() implementation within methods
    prob2:f64,  //standard deviation if required OR second probabiity value for transferring in case that is different from prob1
    x:f64,
    y:f64,
    age:f64,  //Age of host
    time:f64, //Time chicken has spent in facility - start from 0.0 from zone 0
    origin_x:u64,
    origin_y:u64,
    restrict:bool,  //Are the hosts free roaming or not?
    range_x:u64,  //"Internal" GRIDSIZE to simulate caged chickens in side the zone itself, not free roaming within facility ->Now to be taken from Segment
    range_y:u64  //Same as above but for the y direction
}
//Note that if you want to adjust the number of zones, you have to, in addition to adjusting the individual values to your liking per zone, also need to change the slice types below!
//Space
const LISTOFPROBABILITIES:[f64;5] = [0.8,0.75,0.5,0.3,0.15]; //Probability of transfer of samonella per zone - starting from zone 0 onwards
const GRIDSIZE:[[f64;2];5] = [[500.0,2000.0],[1000.0,1000.0],[800.0,800.0],[1000.0,1000.0],[1000.0,1000.0]];
const MAX_MOVE:f64 = 25.0;
const MEAN_MOVE:f64 = 5.0;
const STD_MOVE:f64 = 10.0;
const NO_OF_CHICKENS_PER_SEGMENT:[u8;5] = [50,3,3,3,3];
//Disease 
const TRANSFER_DISTANCE: f64 = 0.70;//maximum distance over which hosts can trasmit diseases to one another
//Host parameters
const PROBABILITY_OF_INFECTION:f64 = 1.0/2500.0; //probability of imported host being infected
const MEAN_AGE:f64 = 5.0*24.0; //Mean age of hosts imported (IN HOURS)
const STD_AGE:f64 = 3.0*24.0;//Standard deviation of host age (when using normal distribution)
const MAX_AGE:f64 = 11.0*24.0; //Maximum age of host accepted (Note: as of now, minimum age is 0.0)
const DEFECATION_RATE:f64 = 1.0; //Number times a day host is expected to defecate
const DEPOSIT_RATE:f64 = 1.0; //Number of times a day host is expected to deposit a consumable deposit
//Transfer parameters
const ages:[f64;5] = [5.0,1.0,1.0,1.0,1.0]; //Time hosts are expected spend in each region minimally
//Collection
const AGE_OF_HOSTCOLLECTION: f64 = 20.0*24.0;  //For instance if you were collecting chickens every 15 days
const COLLECT_DEPOSITS: bool = false;
const AGE_OF_DEPOSITCOLLECTION:f64 = 100.0*24.0; //If you were collecting their eggs every 3 days
const FAECAL_CLEANUP_FREQUENCY:usize = 1; //How many times a day do you want faecal matter to be cleaned up?
//or do we do time collection instead?
const TIME_OF_COLLECTION :f64 = 12.0;
//Resolution
const STEP:[usize;5] = [20,8,8,8,8];  //Unit distance between hosts ->Could be used to make homogeneous zoning (Might not be very flexible a modelling decision)
const HOUR_STEP: f64 = 1.0; //Number of times hosts move per hour
const LENGTH: usize = 1*24; //How long do you want the simulation to be?
//Influx? Do you want new chickens being fed into the scenario everytime the first zone exports some to the succeeding zones?
const INFLUX:bool = true;
const PERIOD_OF_INFLUX:u8 = 24; //How many hours before new batch of hosts are imported?
const PERIOD_OF_TRANSPORT:u8 = 1; //Transport chickens between zones every hour (checking that they fulfill ages requirement of course)
//Restriction?
const RESTRICTION:bool = true;

impl host{
    fn infect(mut vector:Vec<host>,loc_x:u64,loc_y:u64,zone:usize)->Vec<host>{
        if let Some(first_host) = vector.iter_mut().filter(|host_| host_.zone == zone).min_by_key(|host| {
            let dx = host.origin_x as i64 - loc_x as i64;
            let dy = host.origin_y as i64 - loc_y as i64;
            (dx*dx + dy*dy) as u64
        }) 
        {if !first_host.infected{first_host.infected=true;}}
        vector
    }
    fn infect_multiple(mut vector:Vec<host>,loc_x:u64,loc_y:u64,n:usize,zone:usize)->Vec<host>{ //homogeneous application ->Periodically apply across space provided,->Once per location
        let mut filtered_vector: Vec<&mut host> = vector.iter_mut().filter(|host| host.zone == zone).collect();

        filtered_vector.sort_by_key(|host| {
            let dx = host.origin_x as i64 - loc_x as i64;
            let dy = host.origin_y as i64 - loc_y as i64;
            (dx*dx + dy*dy) as u64
        }) ;
        for host in filtered_vector.iter_mut().take(n){
            host.infected = true;
        }
        vector
    }
    fn transport(mut vector:&mut Vec<host>,space:&mut Vec<Zone>, influx: bool){ //Also to change ;size if you change number of zones
        let mut output:Vec<host> = Vec::new();
        for zone in (0..space.len()).rev(){
            let mut __:u32 = space.clone()[zone].capacity;
            if &space[zone].capacity>&0 && zone>0{ //If succeeding zones (obviously zone 0 doesn't have do to this - that needs to be done with a replace mechanism)
                let zone_toedit:&mut Zone = &mut space[zone];
                vector.iter_mut().for_each(|mut x| {
                    if x.zone == zone-1 && x.time>ages[zone-1] && __>0{ //Hosts in previous zone that have spent enough time spent in previous zone
                        //Find the first available segment
                        __ -= 1;
                        space[zone-1] = space[zone-1].clone().subtract(x.origin_x.clone(),x.origin_y); //move host from previous zone
                        // println!("{} capacity for zone {} vs {} for zone {}", &space[zone-1].capacity, zone-1,&space[zone].capacity,zone);
                        x.zone += 1;
                        x.time = 0.0;
                        let mut __:u64 = 0;
                        let mut ___:u64 = 0;
                        // println!("Going to deduct capacity @  zone {} with a capacity of {}", zone,zone_toedit.clone().zone);

                        [x.origin_x,x.origin_y,__,___] = space[zone].add();                            
                    }
                })
            // output.append(&mut vector);
            }
            else if zone == 0 && space[zone].capacity>0 && influx{ //replace mechanism : influx is determined by INFLUX and PERIOD OF INLFUX 
                // let mut zone_0:&mut Zone = &mut space[0];
                for _ in 0..space[zone].clone().capacity as usize{
                    // let [x,y]:[u64;2] = space[zone].add();
                    //Roll probability
                    let mut rng = thread_rng();
                    let roll = Uniform::new(0.0, 1.0);
                    let rollnumber: f64 = rng.sample(roll);
                    let [x,y,range_x,range_y] = space[0].add(); 
                    if rollnumber<PROBABILITY_OF_INFECTION{
                        vector.push(host::new_inf(0,0.2,x as f64,y as f64,RESTRICTION,range_x,range_y));
                    }
                    else{
                        vector.push(host::new(0,0.2,x as f64,y as f64,RESTRICTION,range_x,range_y));
                    }
            }
        }
    }
}



    fn transfer(&self)->bool{ //using prob1 as the probability of contracting disease  (in other words, no separation of events between transferring and capturing disease. If something is infected, it is always infected. Potentially.... the prospective new host will not get infected, but the INFECTED is always viably transferring)
        let mut rng = thread_rng();
        let roll = Uniform::new(0.0, 1.0);
        let rollnumber: f64 = rng.sample(roll);
        // println!("DISEASE   {}",rollnumber);
        rollnumber < self.prob1
    }
    fn new(zone:usize, std:f64,loc_x:f64, loc_y:f64,restriction:bool,range_x:u64,range_y:u64)->host{
        //We shall make it such that the chicken is spawned within the bottom left corner of each "restricted grid" - ie cage
        let prob:f64 = LISTOFPROBABILITIES[zone.clone()];
        //Add a random age generator
        host{infected:false,motile:0,zone:zone,prob1:prob,prob2:std,x:loc_x as f64,y:loc_y as f64,age:normal(MEAN_AGE,STD_AGE,MAX_AGE),time:0.0, origin_x:loc_x as u64,origin_y:loc_y as u64,restrict:restriction,range_x:range_x,range_y:range_y}
    }
    fn new_inf(zone:usize, std:f64,loc_x:f64, loc_y:f64,restriction:bool,range_x:u64,range_y:u64)->host{
        let prob:f64 = LISTOFPROBABILITIES[zone.clone()];
        host{infected:true,motile:0,zone:zone,prob1:prob,prob2:std,x:loc_x as f64,y:loc_y as f64,age:normal(MEAN_AGE,STD_AGE,MAX_AGE),time:0.0, origin_x:loc_x as u64,origin_y:loc_y as u64,restrict:restriction,range_x:range_x,range_y:range_y}
    }
    fn deposit(self, consumable: bool)->host{ //Direct way to lay deposit from host. The function is 100% deterministic and layering a probability clause before this is typically expected
        let zone = self.zone.clone();
        let prob1 = self.prob1.clone();
        let prob2 = self.prob2.clone();
        let x = self.x.clone();
        let y = self.y.clone();
        let inf = self.infected.clone();
        let range_y = self.range_y.clone();
        let range_x = self.range_x.clone();
        let restriction = self.restrict.clone();
        let origin_x = self.origin_x.clone();
        let origin_y = self.origin_y.clone();
        // println!("EGG BEING LAID");
        if consumable{host{infected:inf,motile:1,zone:zone,prob1:prob1,prob2:prob2,x:x,y:y,age:0.0,time:0.0,origin_x:x as u64,origin_y:y as u64,restrict:restriction,range_x:range_x,range_y:range_y}}
        else{host{infected:inf,motile:2,zone:zone,prob1:prob1,prob2:prob2,x:x,y:y,age:0.0,time:0.0,origin_x:x as u64,origin_y:y as u64,restrict:restriction,range_x:range_x,range_y:range_y}}
    }
    fn deposit_all(vector:Vec<host>)->Vec<host>{
        //Below is an example whereby hosts deposit twice a day (fecal matter and laying eggs each once per day as an example)
        let mut vecc:Vec<host> = vector.clone();
        let mut vecc_into: Vec<host> = vector.clone().into_iter().filter(|x| x.motile==0).collect::<Vec<_>>(); //With this re are RETAINING the hosts and deposits within the original vector

        //.map wasn't working so we brute forced a loop
        for ele in vecc_into{
            let mut rng = thread_rng();
            let roll = Uniform::new(0.0,1.0);
            let rollnumber: f64 = rng.sample(roll);
            if rollnumber<DEPOSIT_RATE/24.0{//once per 24h or 1 day rate
                let no:usize = normal(1.0,0.5,2.0) as usize;
                for deposits in 0..no{
                    vecc.push(ele.clone().deposit(true)); //consumable once per day rate
                }
            }
            let mut rng = thread_rng();
            let roll = Uniform::new(0.0,1.0);
            let rollnumber: f64 = rng.sample(roll);
            if rollnumber<DEFECATION_RATE/24.0{
                let no:usize = normal(1.0,0.5,2.0) as usize;
                for deposits in 0..no{
                    vecc.push(ele.clone().deposit(false));//non consumable excrement once per day rate
                }
            }
        }
        vecc
    }
    fn shuffle(mut self)->host{
        if self.motile==0{
            //Whether the movement is negative or positive
            let mut rng = thread_rng();
            let roll = Uniform::new(0.0, 2.4);
            let rollnumber: f64 = rng.sample(roll);
            let mult:f64 = match rollnumber{
                0.0..=0.4 => -1.0,
                0.4..=0.8 => 1.0,
                _ => 0.0
            };
            let mut new_x:f64 = 0.0;
            let mut new_y:f64 = 0.0;
            //use truncated normal distribution (which has been forced to be normal) in order to change the values of x and y accordingly of the host - ie movement
            if self.restrict{
                new_x = limits::min(limits::max(self.origin_x as f64,self.x+mult*normal(MEAN_MOVE,STD_MOVE,MAX_MOVE)),(self.origin_x as f64+self.range_x as f64));
                new_y = limits::min(limits::max(self.origin_y as f64,self.y+mult*normal(MEAN_MOVE,STD_MOVE,MAX_MOVE)),(self.origin_y as f64+self.range_y as f64));
            }else{
                new_x = limits::min(limits::max(0.0,self.x+mult*normal(MEAN_MOVE,STD_MOVE,MAX_MOVE)),GRIDSIZE[self.zone as usize][0]);
                new_y = limits::min(limits::max(0.0,self.y+mult*normal(MEAN_MOVE,STD_MOVE,MAX_MOVE)),GRIDSIZE[self.zone as usize][1]);
            }
            
            host{infected:self.infected,motile:self.motile,zone:self.zone,prob1:self.prob1,prob2:self.prob2,x:new_x,y:new_y,age:self.age+1.0/HOUR_STEP,time:self.time+1.0/HOUR_STEP,origin_x:self.origin_x,origin_y:self.origin_y,restrict:self.restrict,range_x:self.range_x,range_y:self.range_y}}
        else{
            //deposits by hosts do not move obviously, but they DO age, which affects collection
            self.age += 1.0/HOUR_STEP;
            self.time += 1.0/HOUR_STEP;
            self
        }
    }
    fn shuffle_all(vector: Vec<host>)->Vec<host>{
        vector.into_iter().map(|x| x.shuffle()).collect()
    }
    fn dist(host1: &host, host2: &host)->bool{
        let diff_x: f64 = host1.x -host2.x;
        let diff_y: f64 = host1.y - host2.y;
        let t: f64 = diff_x.powf(2.0)+diff_y.powf(2.0);
        /////
        //PRINT STATEMENT
        // if t.powf(0.5)<=TRANSFER_DISTANCE{
        //     println!("{} {} vs {} {}",&host1.x,&host1.y,&host2.x,&host2.y);
        // }
        ////
        t.powf(0.5)<=TRANSFER_DISTANCE && host1.zone == host2.zone
    }
    fn transmit(mut inventory:Vec<host>,time:usize)->Vec<host>{//Current version logic: Once the diseased host passes the "test" in fn transfer, then ALL other hosts within distance contract
        //Locate all infected hosts
        let mut cloneof: Vec<host> = inventory.clone();
        cloneof = cloneof.into_iter().filter_map(|mut x|{
            if x.infected{ //x.transfer is how we internalise the probabilistic nature (not definitive way) that a disease can or cannot spread from an infected individual
                Some(x)
            }else{
                None
            }
        }).collect();
        inventory = inventory.into_iter().filter(|x| !x.infected).collect::<Vec<host>>();    
        inventory = inventory.into_iter().filter_map(|mut x|{
            if cloneof.iter().any(|inf| host::dist(&inf,&x) && inf.zone == x.zone){
                let before = x.infected.clone();
                x.infected=x.transfer();
                if !before && x.infected{
                    if x.x!=0.0 && x.y != 0.0{println!("{} {} {} {}",x.x,x.y,time,x.zone);}
                }
                // println!("{} vs {}",&inf.x,&x.x,&inf.y,&x.y);
                Some(x)
            }else{
                Some(x)
            }
        }).collect();
        inventory.extend(cloneof);
        inventory
    }
    fn cleanup(inventory:Vec<host>)->Vec<host>{
        inventory.into_iter().filter_map(|mut x|{
            if x.motile==2{
                None
            }else{
                Some(x)
            }
        }).collect()
    }
    fn collect(inventory:Vec<host>)->[Vec<host>;2]{   //hosts and deposits potentially get collected
        let mut collection:Vec<host> = Vec::new();
        let vec1:Vec<host> = inventory.into_iter().filter_map(|mut x| {
            // println!("Chicken in zone {}",x.zone);
            if x.motile==0 && x.age>AGE_OF_HOSTCOLLECTION && x.zone == GRIDSIZE.len()-1{
                println!("Collecting host(s)...{} days old",x.age/24.0);
                collection.push(x);
                None
            }else if x.motile == 1 && x.age>AGE_OF_DEPOSITCOLLECTION{
                // println!("Collecting deposit(s)...");
                collection.push(x);
                None
            }else{
                Some(x)
            }
        }).collect();
        [vec1,collection]  //collection vector here to be added and pushed into the original collection vector from the start of the loop! This function merely outputs what should be ADDED to collection!
    }
    fn collect__(inventory:Vec<host>)->[Vec<host>;2]{   //hosts and deposits potentially get collected
        let mut collection:Vec<host> = Vec::new();
        let vec1:Vec<host> = inventory.into_iter().filter_map(|mut x| {
            // println!("Chicken in zone {}",x.zone);
            if x.motile==0 && x.time>TIME_OF_COLLECTION && x.zone == GRIDSIZE.len()-1{
                println!("Collecting host(s)...{} days old",x.age/24.0);
                collection.push(x);
                None
            }else if x.motile == 1 && x.age>AGE_OF_DEPOSITCOLLECTION && COLLECT_DEPOSITS{
                // println!("Collecting deposit(s)...");
                collection.push(x);
                None
            }else{
                Some(x)
            }
        }).collect();
        [vec1,collection]  //collection vector here to be added and pushed into the original collection vector from the start of the loop! This function merely outputs what should be ADDED to collection!
    }    
    fn collect_and_replace(inventory:Vec<host>)->[Vec<host>;2]{   //same as collect but HOSTS GET REPLACED (with a Poisson rate of choosing) - note that this imports hosts, doesn't transfer from earlier zone
        let mut collection:Vec<host> = Vec::new();
        let vec1:Vec<host> = inventory.into_iter().filter_map(|mut x| {
            if x.motile==0 && x.age>AGE_OF_HOSTCOLLECTION&& x.zone == 4{
                // println!("Collecting host(s)...{} days old",x.age/24.0);
                collection.push(x.clone());
                // None
                let mut rng = thread_rng();
                let roll = Uniform::new(0.0, 2.4);
                let rollnumber: f64 = rng.sample(roll);
                if rollnumber<PROBABILITY_OF_INFECTION{
                    Some(host{infected:true,age:normal(MEAN_AGE,STD_AGE,MAX_AGE),time:0.0,..x})
                }else{
                    Some(host{infected:false,age:normal(MEAN_AGE,STD_AGE,MAX_AGE),time:0.0,..x})
                }
            }else if x.motile == 1 && x.age>AGE_OF_DEPOSITCOLLECTION{
                // println!("Collecting deposit(s)...");
                collection.push(x);
                None
            }else{
                Some(x)
            }
        }).collect();
        [vec1,collection]  //collection vector here to be added and pushed into the original collection vector from the start of the loop! This function merely outputs what should be ADDED to collection!
    }
    fn report(inventory:&Vec<host>)->[f64;4]{ //simple function to quickly return the percentage of infected hosts
        let inf: f64 = inventory.clone().into_iter().filter(|x| {
            x.infected && x.motile==0
        }).collect::<Vec<_>>().len() as f64;
        let noofhosts: f64 = inventory.clone().into_iter().filter(|x| {
            x.motile==0
        }).collect::<Vec<_>>().len() as f64;

        let inf2: f64 = inventory.clone().into_iter().filter(|x| {
            x.infected && x.motile==1
        }).collect::<Vec<_>>().len() as f64;
        let noofhosts2: f64 = inventory.clone().into_iter().filter(|x| {
            x.motile==1
        }).collect::<Vec<_>>().len() as f64;        

        [inf/(noofhosts+1.0),inf2/(noofhosts2+1.0),noofhosts,noofhosts2]
    }
    fn zone_report(inventory:&Vec<host>,zone:usize)->[f64;4]{ //simple function to quickly return the percentage of infected hosts
        let mut inventory:Vec<host> = inventory.clone().into_iter().filter(|x|{
            x.zone == zone
        }).collect::<Vec<_>>();
        let inf: f64 = inventory.clone().into_iter().filter(|x| {
            x.infected && x.motile==0
        }).collect::<Vec<_>>().len() as f64;
        let noofhosts: f64 = inventory.clone().into_iter().filter(|x| {
            x.motile==0
        }).collect::<Vec<_>>().len() as f64;

        let inf2: f64 = inventory.clone().into_iter().filter(|x| {
            x.infected && x.motile==1
        }).collect::<Vec<_>>().len() as f64;
        let noofhosts2: f64 = inventory.clone().into_iter().filter(|x| {
            x.motile==1
        }).collect::<Vec<_>>().len() as f64;        

        [inf/(noofhosts+1.0),inf2/(noofhosts2+1.0),noofhosts,noofhosts2]
    }    
    fn generate_in_grid(zone:&mut Zone,hosts:&mut Vec<host>){  //Fill up each segment completely to full capacity in a zone with chickens. Also update the capacity to reflect that there is no more space
        let zone_no:usize = zone.clone().zone;
        zone.segments.iter_mut().for_each(|mut x| {
            for _ in 0..x.capacity.clone() as usize{hosts.push(host::new(zone_no,0.2,x.origin_x as f64,x.origin_y as f64,RESTRICTION,x.range_x,x.range_y));}
            x.capacity = 0;
            zone.capacity = 0;
        });
    }
}


fn main(){
    let mut chickens: Vec<host> = Vec::new();
    let mut feast: Vec<host> =  Vec::new();
    let mut zones:Vec<Zone> = Vec::new();

    //Influx parameter
    let mut influx:bool = false;
    
    //GENERATE CLEAN HOSTS AND ZONES/SUBSETS
    // for i in (0..GRIDSIZE[0][0] as u64).step_by(step){
    //     // println!("{}",i as f64)
    //     for j in (0..GRIDSIZE[0][1] as u64).step_by(step){
    //         chickens.push(host::new(0,0.2,i,j,true,step as u64,step as u64));
    //     }
    // }
    //Initialise with chickens in the first zone only
    for grid in 0..GRIDSIZE.len(){
        zones.push(Zone::generate_empty(grid,[GRIDSIZE[grid][0] as u64,GRIDSIZE[grid][1] as u64],STEP[grid]));
    }
    // let mut segmentation: &mut Vec<Segment> = &mut zone.segments;
    // for x_point in (0..GRIDSIZE[grid][0] as u64).step_by(STEP[grid]){
    //     for y_point in (0..GRIDSIZE[grid][1] as u64).step_by(STEP[grid]){
    //         //EQUIDISTANT GENERATION - HOMOEGENEOUS (to be edited at will later)
    //         //Generate chickens
    //         chickens.push(host::new(0,0.2,x_point,y_point,true,STEP[grid],STEP[grid] ));
    //         capacity[grid] -= 1; //Register the chicken as taking up one capacity
    //         segmentation.push(Segment{zone:grid,origin_x:x_point,origin_y:y_point,range_x:STEP[grid], range_y:STEP[grid], capacity:0})
    //     }
    // }
    // println!("{:?}",chickens.len());
    // for i in zones.clone(){
    //     println!("CAPACITIES: {}",i.capacity);
    // }
    host::generate_in_grid(&mut zones[0],&mut chickens);
    for i in zones.clone(){
        // println!("CAPACITIES: {}",i.capacity);
    }
    // println!("{:?}", chickens.len());
    // for thing in chickens.clone(){
    //     println!("Located at zone {} in {} {}: MOTION PARAMS: {} for {} and {} for {}",thing.zone,thing.x,thing.y,thing.origin_x,thing.range_x,thing.origin_y,thing.range_y);
    // }
    //GENERATE INFECTED HOST
    // chickens.push(host::new_inf(1,0.2,(GRIDSIZE[0] as u64)/2,(GRIDSIZE[1] as u64)/2),true,STEP as u64,STEP as u64); // the infected
    // chickens = host::infect(chickens,400,400,0);
    // chickens = host::infect(chickens,800,800,0);
    // chickens = host::infect(chickens,130,40,0);
    // chickens = host::infect(chickens,10,10,0);
    // chickens = host::infect(chickens,300,1800,0);

    //MORE EFFICIENT WAY TO INFECT MORE CHICKENS
    chickens = host::infect_multiple(chickens,GRIDSIZE[0][0] as u64/2,GRIDSIZE[0][1] as u64/2,10000,0);


    //Count number of infected
    // let it: u64 = chickens.clone().into_iter().filter(|x| x.infected).collect()::<Vec<_>>.len();
    // let mut vecc_into: Vec<host> = chickens.clone().into_iter().filter(|x| x.infected).collect::<Vec<_>>(); //With this re are RETAINING the hosts and deposits within the original vector
    // println!("NUMBER OF INFECTED CHICKENS IS {}", vecc_into.len());
    //CSV FILE
    let filestring: String = format!("./output.csv");
    if fs::metadata(&filestring).is_ok() {
        fs::remove_file(&filestring).unwrap();
    }
    // Open the file in append mode for writing
    let mut file = OpenOptions::new()
    .write(true)
    .create(true)
    .append(true) // Open in append mode
    .open(&filestring)
    .unwrap();
    let mut wtr = Writer::from_writer(file);
    for time in 0..LENGTH{
        let mut collect: Vec<host> = Vec::new();
        for _ in 0..HOUR_STEP as usize{
            chickens = host::shuffle_all(chickens); 
            chickens = host::transmit(chickens,time.clone());
        } //Say chickens move/don't move every 15min - 4 times per hour
        chickens = host::deposit_all(chickens);
        [chickens,collect] = host::collect__(chickens);
        //Collect the hosts and deposits as according
        feast.append(&mut collect);
        if INFLUX && time%PERIOD_OF_INFLUX as usize==0 && time>0{
            influx = true;
            println!("Influx just got changed to true");
        }else{
            influx = false;
        }
        if time%PERIOD_OF_TRANSPORT  as usize==0{
            host::transport(&mut chickens,&mut zones,influx);
        }
        // let mut count:u8 = 0;
        // for i in zones.clone(){
        //     println!("{} : {}", count,i.capacity);
        //     count+=1;
        // }
        //Farm
        let no_of_zones:usize = GRIDSIZE.len();
        let collection_zone_no:u8 = no_of_zones as u8+1;
        //Call once
        for iter in 0..no_of_zones{
            let [mut perc,mut perc2,mut total_hosts,mut total_hosts2] = host::zone_report(&chickens,iter);            
            let no = perc.clone()*total_hosts;
            perc = perc*100.0;
            let no2 = perc2.clone()*total_hosts2;        
            perc2 = perc2*100.0;
            wtr.write_record(&[
                perc.to_string(),
                total_hosts.to_string(),
                no.to_string(),
                perc2.to_string(),
                total_hosts2.to_string(),
                no2.to_string(),
                format!("Zone {}", iter),
            ]);
        }

        //Collection
        let [mut _perc,mut _perc2,mut _total_hosts,mut _total_hosts2] = host::report(&feast);
        _perc = _perc*100.0;
        let _no = _perc*_total_hosts;
        _perc2 = _perc2*100.0;
        let _no2 = _perc2*_total_hosts2;            
        // println!("{} {} {} {} {} {}",perc,total_hosts,no,perc2,total_hosts2,no2);    
        // println!("{} {} {} {} {} {} {} {} {} {} {} {}",perc,total_hosts,no,perc2,total_hosts2,no2,_perc,_total_hosts,_no,_perc2,_total_hosts2,_no2);
        wtr.write_record(&[
            _perc.to_string(),
            _total_hosts.to_string(),
            _no.to_string(),
            _perc2.to_string(),
            _total_hosts2.to_string(),
            _no2.to_string(),
            "Collection Zone".to_string(),
        ])
        .unwrap();
        if time % (24/FAECAL_CLEANUP_FREQUENCY) ==0{
            chickens = host::cleanup(chickens);
        }
        // if host::report(&chickens)[2]<5.0{break;}
    }
    wtr.flush().unwrap();
    println!("{} {} {} {}",GRIDSIZE[0][0],GRIDSIZE[0][1],LENGTH,GRIDSIZE.len()); //Last 5 lines are going to be zone config lines that need to be picked out in plotter.py

    
    // Open a file for writing
    let mut file = File::create("parameters.txt").expect("Unable to create file");

    // Write constants to the file
    // Space
    writeln!(file, "## Space").expect("Failed to write to file");
    writeln!(file, "- LISTOFPROBABILITIES: {:?} (Probability of transfer of salmonella per zone)", LISTOFPROBABILITIES).expect("Failed to write to file");
    writeln!(file, "- GRIDSIZE: {:?} (Size of the grid)", GRIDSIZE).expect("Failed to write to file");
    writeln!(file, "- MAX_MOVE: {} (Maximum move value)", MAX_MOVE).expect("Failed to write to file");
    writeln!(file, "- MEAN_MOVE: {} (Mean move value)", MEAN_MOVE).expect("Failed to write to file");
    writeln!(file, "- STD_MOVE: {} (Standard deviation of move value)", STD_MOVE).expect("Failed to write to file");

    // Disease
    writeln!(file, "\n## Disease").expect("Failed to write to file");
    writeln!(file, "- TRANSFER_DISTANCE: {} (Maximum distance for disease transmission)", TRANSFER_DISTANCE).expect("Failed to write to file");

    // Collection
    writeln!(file, "\n## Collection").expect("Failed to write to file");
    writeln!(file, "- AGE_OF_HOSTCOLLECTION: {} days", AGE_OF_HOSTCOLLECTION/24.0).expect("Failed to write to file");
    writeln!(file, "- AGE_OF_DEPOSITCOLLECTION: {} days", AGE_OF_DEPOSITCOLLECTION/24.0).expect("Failed to write to file");
    writeln!(file, "- FAECAL_CLEANUP_FREQUENCY: {} times per day", 24/FAECAL_CLEANUP_FREQUENCY).expect("Failed to write to file");

    // Resolution
    writeln!(file, "\n## Resolution").expect("Failed to write to file");
    writeln!(file, "- STEP: {} (Chickens per unit distance)", STEP[0]).expect("Failed to write to file");
    writeln!(file, "- HOUR_STEP: {} (Chickens move per hour)", HOUR_STEP).expect("Failed to write to file");
    writeln!(file, "- LENGTH: {} (Simulation duration in hours)", LENGTH).expect("Failed to write to file");


}
