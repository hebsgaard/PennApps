<?php

class BaseRequestResponse implements \JsonSerializable {

    private $sucessfull;
    private $errorMsg;
    private $sucessData;

    function __construct($errorMsg, $sucessData) {
        if ($errorMsg === null || $sucessData === null) {
            $this->errorMsg = $errorMsg;
            $this->sucessData = $sucessData;
            $this->sucessfull = (!($sucessData === null)); //empty array == null .. is true.. fuck you php
        } else {
            $this->errorMsg = "INVALID API RESULT;"; //we have a situation.. a sucess and error ?? not permitted.
            $this->sucessfull = false;
        }
    }

    public static function createSucess($sucessJson) {

        return new \BaseRequestResponse(null, $sucessJson);
    }

    public static function createError($errJson) {
        return new \BaseRequestResponse($errJson, null);
    }

    public function jsonSerialize() {
        $arr = array();
        $arr["sucess"] = $this->sucessfull;
        if ($this->sucessfull) {
            $arr["data"] = $this->sucessData;
        } else {
            $arr["data"] = $this->errorMsg;
        }
        return $arr;
    }

}
