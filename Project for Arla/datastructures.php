<?php

class application implements JsonSerializable {

    /**
     * @param int id
     */
    private $id;

    public function getId() {
        return $this->id;
    }

    public function setId($id) {
        $this->id = $id;
    }

    /**
     * @param String createdAt


     */private $createdAt;

    public function getCreatedAt() {
        return $this->createdAt;
    }

    public function setCreatedAt($createdAt) {
        $this->createdAt = $createdAt;
    }

    /**
     * @param String name


     */private $name;

    public function getName() {
        return $this->name;
    }

    public function setName($name) {
        $this->name = $name;
    }

    /**
     * @param String address


     */private $address;

    public function getAddress() {
        return $this->address;
    }

    public function setAddress($address) {
        $this->address = $address;
    }

    /**
     * @param String postal


     */private $postal;

    public function getPostal() {
        return $this->postal;
    }

    public function setPostal($postal) {
        $this->postal = $postal;
    }

    /**
     * @param String city


     */private $city;

    public function getCity() {
        return $this->city;
    }

    public function setCity($city) {
        $this->city = $city;
    }

    /**
     * @param String telephone


     */private $telephone;

    public function getTelephone() {
        return $this->telephone;
    }

    public function setTelephone($telephone) {
        $this->telephone = $telephone;
    }

    /**
     * @param String email


     */private $email;

    public function getEmail() {
        return $this->email;
    }

    public function setEmail($email) {
        $this->email = $email;
    }

    /**
     * @param String munipicial


     */private $munipicial;

    public function getMunipicial() {
        return $this->munipicial;
    }

    public function setMunipicial($munipicial) {
        $this->munipicial = $munipicial;
    }

    /**
     * @param int smoker


     */private $smoker;

    public function getSmoker() {
        return $this->smoker;
    }

    public function setSmoker($smoker) {
        $this->smoker = $smoker;
    }

    /**
     * @param String school


     */private $school;

    public function getSchool() {
        return $this->school;
    }

    public function setSchool($school) {
        $this->school = $school;
    }

    /**
     * @param String earlierjobs


     */private $earlierjobs;

    public function getEarlierjobs() {
        return $this->earlierjobs;
    }

    public function setEarlierjobs($earlierjobs) {
        $this->earlierjobs = $earlierjobs;
    }

    /**
     * @param Boolean truckcert


     */private $truckcert;

    public function getTruckcert() {
        return $this->truckcert;
    }

    public function setTruckcert($truckcert) {
        $this->truckcert = $truckcert;
    }

    /**
     * @param Boolean cleancert


     */private $cleancert;

    public function getCleancert() {
        return $this->cleancert;
    }

    public function setCleancert($cleancert) {
        $this->cleancert = $cleancert;
    }

    /**
     * @param Boolean knowsap


     */private $knowsap;

    public function getKnowsap() {
        return $this->knowsap;
    }

    public function setKnowsap($knowsap) {
        $this->knowsap = $knowsap;
    }

    /**
     * @param Boolean knowlean


     */private $knowlean;

    public function getKnowlean() {
        return $this->knowlean;
    }

    public function setKnowlean($knowlean) {
        $this->knowlean = $knowlean;
    }

    /**
     * @param Boolean knowenglish


     */private $knowenglish;

    public function getKnowenglish() {
        return $this->knowenglish;
    }

    public function setKnowenglish($knowenglish) {
        $this->knowenglish = $knowenglish;
    }

    /**
     * @param Boolean knowenglishtext


     */private $knowenglishtext;

    public function getKnowenglishtext() {
        return $this->knowenglishtext;
    }

    public function setKnowenglishtext($knowenglishtext) {
        $this->knowenglishtext = $knowenglishtext;
    }

    /**
     * @param String personaldescription


     */private $personaldescription;

    public function getPersonaldescription() {
        return $this->personaldescription;
    }

    public function setPersonaldescription($personaldescription) {
        $this->personaldescription = $personaldescription;
    }

    /**
     * @param String hirereason


     */private $hirereason;

    public function getHirereason() {
        return $this->hirereason;
    }

    public function setHirereason($hirereason) {
        $this->hirereason = $hirereason;
    }

    /**
     * @param String readydate


     */private $readydate;

    public function getReadydate() {
        return $this->readydate;
    }

    public function setReadydate($readydate) {
        $this->readydate = $readydate;
    }

    /**
     * @param String worktype


     */private $worktype;

    public function getWorktype() {
        return $this->worktype;
    }

    public function setWorktype($worktype) {
        $this->worktype = $worktype;
    }

    /**
     * @param Boolean day


     */private $day;

    public function getDay() {
        return $this->day;
    }

    public function setDay($day) {
        $this->day = $day;
    }

    /**
     * @param Boolean night


     */private $night;

    public function getNight() {
        return $this->night;
    }

    public function setNight($night) {
        $this->night = $night;
    }

    /**
     * @param Boolean prevarla


     */private $prevarla;

    public function getPrevarla() {
        return $this->prevarla;
    }

    public function setPrevarla($prevarla) {
        $this->prevarla = $prevarla;
    }

    /**
     * @param String prevarlawhere


     */private $prevarlawhere;

    public function getPrevarlawhere() {
        return $this->prevarlawhere;
    }

    public function setPrevarlawhere($prevarlawhere) {
        $this->prevarlawhere = $prevarlawhere;
    }

    /**
     * @param String prevarlawhat


     */private $prevarlawhat;

    public function getPrevarlawhat() {
        return $this->prevarlawhat;
    }

    public function setPrevarlawhat($prevarlawhat) {
        $this->prevarlawhat = $prevarlawhat;
    }

    /**
     * @param String lastredone


     */private $lastredone;

    public function getLastredone() {
        return $this->lastredone;
    }

    public function setLastredone($lastredone) {
        $this->lastredone = $lastredone;
    }

    /**
     * @param String comment


     */private $comment;

    public function getComment() {
        return $this->comment;
    }

    public function setComment($comment) {
        $this->comment = $comment;
    }

    public function __construct($id, $createdAt, $name, $address, $postal, $city, $telephone, $email, $munipicial, $smoker, $school, $earlierjobs, $truckcert, $cleancert, $knowsap, $knowlean, $knowenglish, $knowenglishtext, $personaldescription, $hirereason, $readydate, $worktype, $day, $night, $prevarla, $prevarlawhere, $prevarlawhat, $lastredone, $comment) {
        $this->id = $id;
        $this->createdAt = $createdAt;
        $this->name = $name;
        $this->address = $address;
        $this->postal = $postal;
        $this->city = $city;
        $this->telephone = $telephone;
        $this->email = $email;
        $this->munipicial = $munipicial;
        $this->smoker = $smoker;
        $this->school = $school;
        $this->earlierjobs = $earlierjobs;
        $this->truckcert = $truckcert;
        $this->cleancert = $cleancert;
        $this->knowsap = $knowsap;
        $this->knowlean = $knowlean;
        $this->knowenglish = $knowenglish;
        $this->knowenglishtext = $knowenglishtext;
        $this->personaldescription = $personaldescription;
        $this->hirereason = $hirereason;
        $this->readydate = $readydate;
        $this->worktype = $worktype;
        $this->day = $day;
        $this->night = $night;
        $this->prevarla = $prevarla;
        $this->prevarlawhere = $prevarlawhere;
        $this->prevarlawhat = $prevarlawhat;
        $this->lastredone = $lastredone;
        $this->comment = $comment;
    }

    public function jsonSerialize() {
        $json = array();
        foreach ($this as $key => $value) {
            $json[$key] = $value;
        }
        return $json;
    }

}
