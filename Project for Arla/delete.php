<?php

include_once 'BaseRequestResponse.php';

function deleteFromDb($id) {
    $sql = "DELETE FROM application WHERE id=$id";
    include 'settings.php';
    $mysqli->query($sql);
}
