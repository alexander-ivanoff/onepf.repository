package org.onepf.repository.model.services.mysql;

/**
 * Created by ivanoff on 25.03.14.
 */
public class SqlOptions {

    // TODO maybe move all setting to separate .properties file
    public String appstoreTable = "appstores";
    public String packageTable = "packages";
    public String purchaseTable = "purchases";
    public String downloadTable = "downloads";

    // sql connection options
    public String driverClassName = "com.mysql.jdbc.Driver";
    public String dbUser = "onepf_user";
    public String dbPassword = "onepfPassword";
    public String dbUrl = "jdbc:mysql://onepf-repository.ca6s4oee7dbi.us-west-2.rds.amazonaws.com:3306/onepf_repository";
    public int maxConnections = 4;


}