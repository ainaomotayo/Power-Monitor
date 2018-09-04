package com.wolfram.databaselink;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.Statement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.Time;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Calendar;

import com.wolfram.jlink.Expr;

public class SQLStatementProcessor
{

  private static final Expr SYM_SQLBINARY = new Expr(Expr.SYMBOL, "SQLBinary");
  private static final Expr SYM_SQLDATETIME = new Expr(Expr.SYMBOL, "SQLDateTime");
  private static final Expr SYM_SQLEXPR = new Expr(Expr.SYMBOL, "SQLExpr");
  private static final Expr SYM_NULL = new Expr(Expr.SYMBOL, "Null");

  public static Object[] processSQLStatement(
    Connection connection,
    String sql,
    Expr params,
    boolean getAsStrings,
    boolean showColumnHeadings,
    boolean returnResultSet,
    boolean returnGeneratedKeys,
    boolean useLongs,
    int maxrows,
    int timeout,
    int resultSetType,
    int resultSetConcurrency, 
    int escapeProcessing,
    int fetchDirection, 
    int fetchSize,
    int maxFieldSize,
    int batchSize) throws Exception
  {
    PreparedStatement ps = null;
    
    if(returnGeneratedKeys)
      ps = connection.prepareStatement(sql, 1);
    else
      ps = connection.prepareStatement(sql, resultSetType, resultSetConcurrency);

    boolean k = false;
    //int [] intArray = null;
    ArrayList<Integer> res = new ArrayList<Integer>();
    boolean success = true;     // for batch adds

    if(maxrows > 0)
      ps.setMaxRows(maxrows);

    if(timeout > 0)
      ps.setQueryTimeout(timeout);

    if(fetchDirection > 0)
        ps.setFetchDirection(fetchDirection);

    if(fetchSize != 0)
        ps.setFetchSize(fetchSize);

    if(maxFieldSize > 0)
        ps.setMaxFieldSize(maxFieldSize);
    
    if(escapeProcessing >= 0)
        // Setting this to false appears to be pointless for prepared statements
        ps.setEscapeProcessing((escapeProcessing == 1) ? true:false);

    // Mem use implications of this loop?
    for(int h = 1; h <= params.length(); h++)
    {
      Expr list = params.part(h);
      for(int i = 1; i <= list.length(); i++)
      {
        Expr e = list.part(i);
        if(e.realQ())
          ps.setDouble(i, e.asDouble());
        else if(e.integerQ())
          if(useLongs)
              ps.setLong(i, e.asLong());
          else
              ps.setInt(i, e.asInt());
        else if(e.stringQ())
          ps.setString(i, e.asString());
        else if(e.equals(Expr.SYM_TRUE) || e.equals(Expr.SYM_FALSE))
          ps.setBoolean(i, e.trueQ());
        else if(e.equals(SYM_NULL))
          ps.setNull(i, Types.NULL);

        else if(e.head().equals(SYM_SQLBINARY))
        {
          if(e.part(1).vectorQ(Expr.INTEGER))
          {
            int[] a = (int[])e.part(1).asArray(Expr.INTEGER, 1);
            byte[] bytes = new byte[a.length];
            for(int j = 0; j < a.length; j++)
            {
              if( a[j] > 127)
                bytes[j] = (byte)(a[j] - 256);
              else
                bytes[j] = (byte)a[j];
            }
            ps.setBytes(i, bytes);
          }
          else
          {
            byte[] bytes = new byte[e.length()];
            for(int j = 1; j <= e.length(); j++)
            { 
              Expr a = e.part(j);
              if(a.integerQ())
              {
                int b = a.asInt();
                if( b > 127)
                  bytes[j-1] = (byte)(b - 256);
                else
                  bytes[j-1] = (byte)b;
              }
              else
              {
                throw new Exception("SQLBinary may only contain integers from 0 to 255.");
              }
            }
            ps.setBytes(i, bytes);
          }
        }
        else if(e.head().equals(SYM_SQLDATETIME))
        {
          if(e.part(1).listQ())
            e = e.part(1);
            Calendar cal = Calendar.getInstance();
          if(e.length() == 6)
          {
            int nanval = 0;
            if(e.part(1).integerQ())
              cal.set(Calendar.YEAR, e.part(1).asInt());
            else
              throw new Exception("Illegal value for year in SQLDateTime: " + e.part(1).toString());
            if(e.part(2).integerQ())
              cal.set(Calendar.MONTH, e.part(2).asInt()-1);
            else
              throw new Exception("Illegal value for month in SQLDateTime: " + e.part(2).toString());
            if(e.part(3).integerQ())
              cal.set(Calendar.DATE, e.part(3).asInt());
            else
              throw new Exception("Illegal value for date in SQLDateTime: " + e.part(3).toString());
            if(e.part(4).integerQ())
              cal.set(Calendar.HOUR_OF_DAY, e.part(4).asInt());
            else
              throw new Exception("Illegal value for hour in SQLDateTime: " + e.part(4).toString());
            if(e.part(5).integerQ())
              cal.set(Calendar.MINUTE, e.part(5).asInt());
            else
              throw new Exception("Illegal value for minute in SQLDateTime: " + e.part(5).toString());
            if(e.part(6).realQ())
            {
              double dbval = e.part(6).asDouble();
              int secval = new Double(dbval).intValue();
              nanval = new Double((dbval - secval) * 1000000000).intValue();
              cal.set(Calendar.SECOND, secval);
            }
            else if(e.part(6).integerQ())
              cal.set(Calendar.SECOND, e.part(6).asInt());
            else
              throw new Exception("Illegal value for second in SQLDateTime: " + e.part(6).toString());
            Timestamp ts = new Timestamp(cal.getTime().getTime());
            ts.setNanos(nanval);
            ps.setTimestamp(i, ts);
          }
          else if(e.length() == 3)
          {
            if(e.part(1).integerQ() && e.part(1).asInt() > 24)
            {
              cal.set(Calendar.YEAR, e.part(1).asInt());
              if(e.part(2).integerQ())
                cal.set(Calendar.MONTH, e.part(2).asInt()-1);
              else
                throw new Exception("Illegal value for month in SQLDateTime: " + e.part(2).toString());
              if(e.part(3).integerQ())
                cal.set(Calendar.DATE, e.part(3).asInt());
              else
                throw new Exception("Illegal value for date in SQLDateTime: " + e.part(3).toString());
              
              cal.set(Calendar.HOUR_OF_DAY, 0);
              cal.set(Calendar.MINUTE, 0);
              cal.set(Calendar.SECOND, 0);
              Timestamp ts = new Timestamp(cal.getTime().getTime());
              ts.setNanos(0);
              ps.setTimestamp(i, ts);
            }
            else if(e.part(1).integerQ())
            {
              cal.set(Calendar.HOUR_OF_DAY, e.part(1).asInt());
              if(e.part(2).integerQ())
                cal.set(Calendar.MINUTE, e.part(2).asInt());
              else
                throw new Exception("Illegal value for minute in SQLDateTime: " + e.part(2).toString());
              if(e.part(3).integerQ())
                cal.set(Calendar.SECOND, e.part(3).asInt());
              else
                throw new Exception("Illegal value for second in SQLDateTime: " + e.part(3).toString());
              Timestamp ts = new Timestamp(cal.getTime().getTime());
              ts.setNanos(0);
              ps.setTimestamp(i, ts);
              //Time time = new Time(cal.getTime().getTime());
              //ps.setTime(i, time);
            }
            else
              throw new Exception("Illegal value: " + e.toString());
          }
          else
            throw new Exception("Illegal value: " + e.toString());
        }
        else if(e.head().equals(SYM_SQLEXPR))
        {
          ps.setString(i, e.part(1).asString());
        }
        else
          throw new Exception("Illegal value: " + e.toString());
      }
      if(batchSize > 0 && params.length() > 1)
      {
        try
        {
          ps.addBatch();
        } 
        catch(Exception e)
        {
          success = false;
          //if(intArray == null)
            //intArray = new int[params.length()];
          batchSize = 0;
          ps.execute();
          //intArray[h-1] = ps.getUpdateCount();
          res.add(ps.getUpdateCount());
        }
        finally
        {
          if(success && (h % batchSize == 0))
          {
            // executeBatch() returns an array of update counts
            for(int i:ps.executeBatch())
              res.add(i);
          }
        }
      }
      else
      {
        //if(intArray == null)
          //intArray = new int[params.length()];
        k = ps.execute();
        if(!k)
        {
          //intArray[h-1] = ps.getUpdateCount();
          res.add(ps.getUpdateCount());
        }
      } 
    }

    if(batchSize > 0 && params.length() > 1)
    {
      //intArray = ps.executeBatch();
      for(int i:ps.executeBatch())
          res.add(i);
    }

    ResultSet rs = null;
    /* Generated Keys */
    if(returnGeneratedKeys) 
    {
        rs =  ps.getGeneratedKeys();
        if(returnResultSet)
          return new ResultSet[] { rs };
        Object[] results = getAllResultData(rs, getAsStrings, showColumnHeadings);
        ps.close();
        return results;
    }
    /* select statements */
    else if(k)
    {
      rs =  ps.getResultSet();
      if(returnResultSet)
        return new ResultSet[] { rs };
      Object[] results = getAllResultData(rs, getAsStrings, showColumnHeadings);
      ps.close();
      return results;
    }
    /* batch statements */
    else if(!res.isEmpty())
    {
      /*
      Integer[] integerArray = new Integer[intArray.length];
      for(int l = 0; l < intArray.length; l++)
      {
        integerArray[l] = new Integer(intArray[l]);
      }*/
      Integer[] integerArray = new Integer[ res.size() ];
      res.toArray( integerArray );
      ps.close();
      return integerArray;
    }
    /* insert, update, remove statements */
    int updateCount = ps.getUpdateCount();
    ps.close();
    return new Integer[] { new Integer(updateCount) };
  }


  /* Introduced to support the streaming result set settings for MySQL
   * (prepared statements don't work for this). Overkill but might have future applications.
   * 26 Sept 2013 | dillont
   */
  public static Object[] processUnpreparedSQLStatement(
          Connection connection,
          String sql,
          boolean getAsStrings,
          boolean showColumnHeadings,
          boolean returnResultSet,
          boolean returnGeneratedKeys,
          int maxrows,
          int timeout,
          int resultSetType,
          int resultSetConcurrency, 
          int escapeProcessing,
          int fetchDirection, 
          int fetchSize,
          int maxFieldSize) throws Exception
  {
      Statement s = null;
      s = connection.createStatement(resultSetType, resultSetConcurrency);

      boolean k = false;
      //ArrayList<Integer> res = new ArrayList<Integer>();

      if(maxrows > 0)
          s.setMaxRows(maxrows);

      if(timeout > 0)
          s.setQueryTimeout(timeout);

      if(fetchDirection > 0)
          s.setFetchDirection(fetchDirection);

      if(fetchSize != 0)
          s.setFetchSize(fetchSize);

      if(maxFieldSize > 0)
          s.setMaxFieldSize(maxFieldSize);
          
      if(escapeProcessing >= 0)
          s.setEscapeProcessing((escapeProcessing == 1) ? true:false);

      // Accommodate SQLite supported interface
      //k = s.execute(sql, returnGeneratedKeys ? Statement.RETURN_GENERATED_KEYS:Statement.NO_GENERATED_KEYS);
      if(returnGeneratedKeys)
          k = s.execute(sql, Statement.RETURN_GENERATED_KEYS);
      else
          k = s.execute(sql);

      ResultSet rs = null;
      /* Generated Keys */
      if(returnGeneratedKeys) 
      {
          rs = s.getGeneratedKeys();
          if(returnResultSet)
              return new ResultSet[] { rs };
          Object[] results = getAllResultData(rs, getAsStrings, showColumnHeadings);
          s.close();
          return results;
      }
      /* select statements */
      else if(k)
      {
          rs =  s.getResultSet();
          if(returnResultSet)
              return new ResultSet[] { rs };
          Object[] results = getAllResultData(rs, getAsStrings, showColumnHeadings);
          s.close();
          return results;
      }
      /* insert, update, remove statements */
      int updateCount = s.getUpdateCount();
      s.close();
      return new Integer[] { new Integer(updateCount) };
  }

  
  public static Object[] getHeadings(ResultSet rs, boolean tables) throws Exception
  {
      ResultSetMetaData meta = rs.getMetaData();
      Object[] headings = new Object[meta.getColumnCount()];
      for(int i = 0; i < meta.getColumnCount(); i++)
      {
        if(tables)
        {
            String[] col = new String[2];
            col[0] = meta.getTableName(i+1);
            // getColumnName will ignore aliases in MySQL | dillont
            col[1] = meta.getColumnLabel(i+1);
            headings[i] = col;
        }
        else
          // getColumnName will ignore aliases in MySQL | dillont
          headings[i] = meta.getColumnLabel(i+1);
      }      
      return headings;
  }
  
  public static Object[] getLimitedResultData(
          int limit, 
          ResultSet rs, 
          boolean getAsStrings) throws Exception
  {
      
    ArrayList<Object> data = new ArrayList<Object>();
    int[] columnTypes = getColumnTypes(rs);
    
    boolean valid = false;    
    if(limit == 0)
    {
      Object[] row = getRow(rs, columnTypes, getAsStrings);
      data.add(row);        
    }
    if(limit > 0)
    {
      Object[] row;
      for(int j = 0; j < limit; j++)
      {
        valid = rs.next();
        if(!valid)
          break;
        row = getRow(rs, columnTypes, getAsStrings);
        data.add(row);
      }
    }
    if(limit < 0)
    {    
      Object[] row;
      for(int k = 0; k > limit; k--)
      {
        valid = rs.previous();
        if(!valid)
          break;
        row = getRow(rs, columnTypes, getAsStrings);
        data.add(row);            
      }        
    }        
    if(data.size() == 0 && !valid)
      return null;
      
    return data.toArray(new Object[data.size()]);
  }

  public static Object[] getAllResultData(
          ResultSet rs, 
          boolean getAsStrings, 
          boolean showColumnHeadings) throws Exception
  {
    boolean valid = rs.next();
    
    ArrayList<Object> data = new ArrayList<Object>();
    if(showColumnHeadings)
        data.add(getHeadings(rs, false));
    int[] columnTypes = getColumnTypes(rs);
    
    while(valid)
    {
      Object[] row = getRow(rs, columnTypes, getAsStrings);
      data.add(row);
      valid = rs.next();            
    }
    
    return data.toArray(new Object[data.size()]);
  }
  
  private static Object[] getRow(ResultSet rs, int[] columnTypes, boolean getAsStrings) throws Exception
  {
    int cc = columnTypes.length;
    Object[] row = new Object[cc];
    if(getAsStrings)
    {
      for(int j = 0; j < cc; j++)
      {
        row[j] = rs.getString(j+1);
      }
    }
    else
    {
      for(int j = 0; j < cc; j++)
      {
        int ct = columnTypes[j];
        if (ct == 4 || ct == -7 || ct == 16 || ct == 6 || ct == 8 || ct == -5 || ct == 7 ||
            ct == 5 || ct == -6 || ct == 2 || ct == 3)
          row[j] = rs.getObject(j+1);  
        else if (ct == -2 || ct == -3 || ct == -4)
        {
          byte[] bytes = rs.getBytes(j+1);
          if(bytes != null)
          {
            int[] a = new int[bytes.length];
            for(int k = 0; k < bytes.length; k ++)
            {
              if(bytes[k] < 0)
                a[k] = bytes[k] + 256;
              else
                a[k] = bytes[k];
            }
            row[j] = new Expr(new Expr(Expr.SYMBOL, "SQLBinary"), new Expr[] {new Expr(a)});
          }
          else
          {
            row[j] = SYM_NULL;
          }
        }
        else if (ct == 91)
        {
          Date d = rs.getDate(j+1);
          if(d != null)
          {
            Calendar cal = Calendar.getInstance();
            cal.setTime(new Date(d.getTime()));
            row[j] = new Expr(
               new Expr(Expr.SYMBOL, "SQLDateTime"),
               new Expr[]
               {
                 new Expr(
                   new Expr(Expr.SYMBOL, "List"),
                   new Expr[] {
                     new Expr(cal.get(Calendar.YEAR)),
                     new Expr(cal.get(Calendar.MONTH)+1),
                     new Expr(cal.get(Calendar.DATE))
                   })
               });
          }
          else
            row[j] = SYM_NULL;
        }
        else if (ct == 92)
        {
          Time t = rs.getTime(j+1);
          if(t != null)
          {
            Calendar cal = Calendar.getInstance();
            cal.setTime(new Date(t.getTime()));
            row[j] = new Expr(
               new Expr(Expr.SYMBOL, "SQLDateTime"),
               new Expr[] {
                 new Expr(
                   new Expr(Expr.SYMBOL, "List"),
                   new Expr[]
                     {
                       new Expr(cal.get(Calendar.HOUR_OF_DAY )),
                       new Expr(cal.get(Calendar.MINUTE)),
                       new Expr(cal.get(Calendar.SECOND))
                     })
               });
          }
          else
            row[j] = SYM_NULL;
        }
        else if (ct == 93)
        {
          Timestamp ts = rs.getTimestamp(j+1);
          if(ts != null)
          {
            Calendar cal = Calendar.getInstance();
            cal.setTime(new Date(ts.getTime()));
            row[j] = new Expr(
               new Expr(Expr.SYMBOL, "SQLDateTime"),
               new Expr[]
               {
                 new Expr(
                   new Expr(Expr.SYMBOL, "List"),
                   new Expr[] {
                     new Expr(cal.get(Calendar.YEAR)),
                     new Expr(cal.get(Calendar.MONTH)+1),
                     new Expr(cal.get(Calendar.DATE)),
                     new Expr(cal.get(Calendar.HOUR_OF_DAY )),
                     new Expr(cal.get(Calendar.MINUTE)),
                     new Expr(cal.get(Calendar.SECOND)+(new Integer(ts.getNanos()).doubleValue()/1000000000))
                   })
               });
          } else
            row[j] = SYM_NULL;
        }
        else
        {
          String val = rs.getString(j+1);
          if(val != null && val.startsWith("SQLExpr["))
              row[j] = new Expr(
                         new Expr(Expr.SYMBOL, "ToExpression"),
                         new Expr[]{ new Expr(val) });
          else
            row[j] = val;
        }
      }
    }
    return row;
  }
  
  private static int[] getColumnTypes(ResultSet rs) throws Exception
  {
      ResultSetMetaData meta = rs.getMetaData();
      int cc = meta.getColumnCount();
      int[] columnTypes = new int[cc];
      for(int j = 0; j < cc; j++)
      {
        columnTypes[j] = meta.getColumnType(j+1);
      }
      return columnTypes;
  }
  
  public static Object[] getConnectionMetaData(Connection conn) throws Exception
  {
	  Object[] data = new Object[2];

	  int mdiCount = MetaDataItem.values().length;
	  Object[] mdiNames = new Object[mdiCount];
	  Object[] mdiValues = new Object[mdiCount];
	  
	  DatabaseMetaData metaData = conn.getMetaData();
	  
	  int itemCount = 0;
	  for (MetaDataItem mdi : MetaDataItem.values())
	  {
		  mdiNames[itemCount] = mdi.name();
		  mdiValues[itemCount] = mdi.getValue(metaData);
		  itemCount++;
	  }
	  
	  data[0] = mdiNames;
	  data[1] = mdiValues;
	  
	  return data;
  }

  public static Object[] getConnectionMetaData(Connection conn, String[] mdiListRequested) throws Exception
  {
	  Object[] data = new Object[2];
  	  
	  int mdiListCount = mdiListRequested.length;
	  Object[] mdiNames = new Object[mdiListCount];
	  Object[] mdiValues = new Object[mdiListCount];
	  
	  DatabaseMetaData metaData = conn.getMetaData();
	  
	  for (int itemCount = 0; itemCount < mdiListCount; itemCount++)
	  {
		  MetaDataItem mdi = MetaDataItem.valueOf(mdiListRequested[itemCount]);

		  mdiNames[itemCount] = mdi.name();
		  mdiValues[itemCount] = mdi.getValue(metaData);
		  
	  }
	  
	  data[0] = mdiNames;
	  data[1] = mdiValues;
	  
	  return data;	  
  }

  public static Object getConnectionMetaData(Connection conn, String mdiRequested) throws Exception
  {
	  Object data = new Object();
	  
	  DatabaseMetaData metaData = conn.getMetaData();
	  
	  MetaDataItem mdi = MetaDataItem.valueOf(mdiRequested);
	  
	  data = mdi.getValue(metaData);
	  
	  return data;
  }
  
}