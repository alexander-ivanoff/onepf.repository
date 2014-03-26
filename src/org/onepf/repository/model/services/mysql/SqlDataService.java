package org.onepf.repository.model.services.mysql;

import org.apache.commons.dbcp.ConnectionFactory;
import org.apache.commons.dbcp.DriverManagerConnectionFactory;
import org.apache.commons.dbcp.PoolableConnectionFactory;
import org.apache.commons.dbcp.PoolingDataSource;
import org.apache.commons.pool.ObjectPool;
import org.apache.commons.pool.impl.GenericObjectPool;
import org.onepf.repository.model.auth.AppstoreDescriptor;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.mysql.entities.*;
import org.onepf.repository.utils.responsewriter.descriptors.ApplicationDescriptor;
import org.onepf.repository.utils.responsewriter.descriptors.DownloadDescriptor;
import org.onepf.repository.utils.responsewriter.descriptors.PurchaseDescriptor;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.*;

/**
 * Created by ivanoff on 18.03.14.
 */
public class SqlDataService implements DataService {

    private DataSource dbDataSource;

    public SqlDataService(SqlOptions options) {

        // init DBCP DataSource
        dbDataSource = setupDataSource(options);

    }

    public static DataSource setupDataSource(SqlOptions options) {

        try {
            Class.forName(options.driverClassName);
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
        ObjectPool connectionPool = new GenericObjectPool(null, options.maxConnections, GenericObjectPool.WHEN_EXHAUSTED_BLOCK, options.maxConnections);
        ConnectionFactory connectionFactory = new DriverManagerConnectionFactory(options.dbUrl, options.dbUser, options.dbPassword);
        PoolableConnectionFactory poolableConnectionFactory = new PoolableConnectionFactory (connectionFactory,connectionPool,null,null,false,true);
        PoolingDataSource dataSource = new PoolingDataSource(connectionPool);
        return dataSource;
    }

    @Override
    public void store(ApplicationDescriptor applicationDescriptor) throws DataException {
        Connection conn = null;
        PreparedStatement stmt = null;
        try {
            SqlAppEntity appEntity = new SqlAppEntity()
                    .withPackageName(applicationDescriptor.packageName)
                    .withLastUpdate(applicationDescriptor.lastUpdated)
                    .withDevelopersContact(applicationDescriptor.developerContact)
                    .withAppstore(applicationDescriptor.appstoreId);
            conn = dbDataSource.getConnection();
            stmt = insert(conn, "applications", appEntity);
            stmt.executeUpdate();
        } catch (SQLException e) {
            throw new DataException(e);
        } finally {
            try { if (stmt != null) stmt.close(); } catch(Exception e) { }
            try { if (conn != null) conn.close(); } catch(Exception e) { }
        }

    }

    @Override
    public List<ApplicationDescriptor> getApplications() throws DataException{
        Connection conn = null;
        PreparedStatement stmt = null;
        ResultSet rset = null;
        try {
            conn = dbDataSource.getConnection();
            stmt = query(conn, "applications", null, null);
            rset = stmt.executeQuery();
            ArrayList<ApplicationDescriptor> apps = new ArrayList<ApplicationDescriptor>();
            while (rset.next()) {
                apps.add(SqlAppEntity.getDescriptor(rset));
            }
            return apps;
        } catch (SQLException e) {
            throw new DataException(e);
        } finally {
            try { if (stmt != null) rset.close(); } catch(Exception e) { }
            try { if (stmt != null) stmt.close(); } catch(Exception e) { }
            try { if (conn != null) conn.close(); } catch(Exception e) { }
        }
    }

    @Override
    public Map<String, AppstoreDescriptor> getAppstores() throws DataException{
        Connection conn = null;
        PreparedStatement stmt = null;
        ResultSet rset = null;
        try {
            conn = dbDataSource.getConnection();
            stmt = query(conn, "appstores", null, null);
            rset = stmt.executeQuery();
            Map<String, AppstoreDescriptor> apps = new HashMap<String, AppstoreDescriptor>();
            AppstoreDescriptor appstore = null;
            while (rset.next()) {
                appstore = SqlAppstoreEntity.getDescriptor(rset);
                apps.put(appstore.authToken, appstore);
            }
            return apps;
        } catch (SQLException e) {
            e.printStackTrace();
            throw new DataException(e);
        } finally {
            try { if (stmt != null) rset.close(); } catch(Exception e) { }
            try { if (stmt != null) stmt.close(); } catch(Exception e) { }
            try { if (conn != null) conn.close(); } catch(Exception e) { }
        }
    }

    @Override
    public ArrayList<DownloadDescriptor> getDownloads(String packageName, long updateTime) throws DataException {
        Connection conn = null;
        PreparedStatement stmt = null;
        ResultSet rset = null;
        try {
            String selection = SqlDownloadEntity.FIELD_PACKAGE_NAME + "=? AND " + SqlDownloadEntity.FIELD_DATE_TIME + ">=?";
            String[] selectionArgs = new String[] {packageName, String.valueOf(updateTime)};
            conn = dbDataSource.getConnection();
            stmt = query(conn, "applications", selection, selectionArgs);
            ArrayList<DownloadDescriptor> downloads = new ArrayList<DownloadDescriptor>();
            while (rset.next()) {
                downloads.add(SqlDownloadEntity.getDescriptor(rset));
            }
            return downloads;
        } catch (SQLException e) {
            throw new DataException(e);
        } finally {
            try { if (stmt != null) rset.close(); } catch(Exception e) { }
            try { if (stmt != null) stmt.close(); } catch(Exception e) { }
            try { if (conn != null) conn.close(); } catch(Exception e) { }
        }
    }

    @Override
    public ArrayList<PurchaseDescriptor> getPurchases(String packageName, long updateTime) throws DataException {
        Connection conn = null;
        PreparedStatement stmt = null;
        ResultSet rset = null;
        try {
            String selection = SqlPurchaseEntity.FIELD_PACKAGE_NAME + "=? AND " + SqlPurchaseEntity.FIELD_DATE_TIME + ">=?";
            String[] selectionArgs = new String[] {packageName, String.valueOf(updateTime)};
            conn = dbDataSource.getConnection();
            stmt = query(conn, "applications", selection, selectionArgs);
            ArrayList<PurchaseDescriptor> purchases = new ArrayList<PurchaseDescriptor>();
            while (rset.next()) {
                purchases.add(SqlPurchaseEntity.getDescriptor(rset));
            }
            return purchases;
        } catch (SQLException e) {
            throw new DataException(e);
        } finally {
            try { if (stmt != null) rset.close(); } catch(Exception e) { }
            try { if (stmt != null) stmt.close(); } catch(Exception e) { }
            try { if (conn != null) conn.close(); } catch(Exception e) { }
        }
    }

    private static PreparedStatement insert(Connection connection,  String tableName, SqlDBEntity dbEntity) throws SQLException {

        StringBuilder columnsBuilder = new StringBuilder().append("(");
        StringBuilder valuesBuilder = new StringBuilder().append("(");
        Map<String, String> item = dbEntity.getItem();
        for (String entry : item.keySet()) {
            columnsBuilder.append(entry).append(',');
            valuesBuilder.append('?').append(',');

        }
        columnsBuilder.replace(columnsBuilder.length() - 1, columnsBuilder.length(), ")");
        valuesBuilder.replace(valuesBuilder.length() - 1, valuesBuilder.length(), ")");
        String request = "REPLACE INTO " + tableName + " " + columnsBuilder.toString() + " VALUES " + valuesBuilder.toString();
        System.out.println("QUERY: " + request);
        PreparedStatement stmt = null;

            stmt = connection.prepareStatement(request);
            Collection<String> values = item.values();
            int index = 0;
            for (String value: values) {
                stmt.setString(++index, value);
            }
            return  stmt;

    }

    private static PreparedStatement query(Connection connection,  String tableName, String selection, String[] selectionArgs) throws SQLException {

        StringBuilder requestBuilder = new StringBuilder().append("SELECT * FROM ").append(tableName);
        if (selection != null) {
            requestBuilder.append(" WHERE ").append(selection);
        };
        requestBuilder.append(" LIMIT 1000");
        System.out.println("QUERY: " + requestBuilder.toString());

        PreparedStatement stmt = null;
        try {
            stmt = connection.prepareStatement(requestBuilder.toString());
            int index = 0;
            if (selectionArgs != null) {
                for (String value: selectionArgs) {
                    stmt.setString(++index, value);
                }
            }
            return stmt;
        } catch (SQLException e) {
            e.printStackTrace();
            throw e;
        }
    }

}
