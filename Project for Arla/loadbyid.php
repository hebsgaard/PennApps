<?php

include_once 'BaseRequestResponse.php';
include_once 'settings.php';



//if (defined("TAPI") === false) {
//    include_once '../api/includer.php';
//    \api\loadAll();
//}
$id = intval(getGet("id"));
$myResult = $mysqli->query("SELECT * FROM application WHERE id=$id ");

$resArr = array();

if ($myResult->num_rows > 0) {
    $row = $myResult->fetch_assoc();
    while ($row != null) {
        $cls = new application( $row["id"], $row["createdAt"], $row["name"], $row["address"], $row["postal"], $row["city"], $row["telephone"], $row["email"], $row["munipicial"], $row["smoker"], $row["school"], $row["earlierjobs"], $row["truckcert"], $row["cleancert"], $row["knowsap"], $row["knowlean"], $row["knowenglish"], $row["knowenglishtext"], $row["personaldescription"], $row["hirereason"], $row["readydate"], $row["worktype"], $row["day"], $row["night"], $row["prevarla"], $row["prevarlawhere"], $row["prevarlawhat"], $row["lastredone"], $row["comment"]);
        $resArr[] = $cls;
        $row = $myResult->fetch_assoc();
    }
}
return $resArr;
