<%-- Created by IntelliJ IDEA. --%>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
<head>
    <title>Upload</title>
</head>
<body>
<form action="openaep/upload" method="post" enctype="multipart/form-data">
    Token: <input name="authToken" type="text"><br>
    developer's contact: <input name="developersContact" type="text"><br>
    <input name="data" type="file"><br>
    <input type="submit"><br>
</form>
</body>
</html>