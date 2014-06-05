<?php
include_once 'BaseRequestResponse.php';
include_once 'settings.php';
include_once '../api/includer.php';
\api\loadAll();
$name = \getGet("name");
$address = \getGet("address");
$postal = \getGet("postal");
$city = \getGet("city");
$telephone = \getGet("telephone");
$email = \getGet("email");
$munipicial = \getGet("munipicial");
$smoker = \getGet("smoker");
$school = \getGet("school");
$earlierjobs = \getGet("earlierjobs");
$truckcert = \getGet("truckcert");
$cleancert = \getGet("cleancert");
$knowsap = \getGet("knowsap");
$knowlean = \getGet("knowlean");
$knowenglish = \getGet("knowenglish");
$knowenglishtext = \getGet("knowenglishtext");
$personaldescription = \getGet("personaldescription");
$hirereason = \getGet("hirereason");
$readydate = \getGet("readydate");
$worktype = \getGet("worktype");
$day = \getGet("day");
$night = \getGet("night");
$prevarla = \getGet("prevarla");
$prevarlawhere = \getGet("prevarlawhere");
$prevarlawhat = \getGet("prevarlawhat");
$lastredone = \getGet("lastredone");
$comment = \getGet("comment");
$myResult = $mysqli->query("INSERT INTO application (createdAt,name,address,postal,city,telephone,email,munipicial,smoker,school,earlierjobs,truckcert,cleancert,knowsap,knowlean,knowenglish,knowenglishtext,personaldescription,hirereason,readydate,worktype,day,night,prevarla,prevarlawhere,prevarlawhat,lastredone,comment ) VALUES( NOW(),\"$name\",\"$address\",\"$postal\",\"$city\","
        . "\"$telephone\",\"$email\",\"$munipicial\",\"$smoker\",\"$school\",\"$earlierjobs\",\"$truckcert\",\"$cleancert\",\"$knowsap\",\"$knowlean\",\"$knowenglish\",\"$knowenglishtext\",\"$personaldescription\",\"$hirereason\",\"$readydate\",\"$worktype\",\"$day\",\"$night\",\"$prevarla\",\"$prevarlawhere\",\"$prevarlawhat\",\"$lastredone\",\"$comment\")");

var_dump($mysqli->error);

?>
<script > alert("Ansøgning oprettet!");</script>

<script> window.location.assign("add.php");</script>