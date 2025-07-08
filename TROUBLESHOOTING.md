# WaveQTEX Troubleshooting Guide

## GitHub Actions Data Pipeline

### How to Check if GitHub Actions is Working

1. **Visit GitHub Actions Page**:
   - Go to: `https://github.com/avishekb9/WaveQTEX/actions`
   - Look for "Update Web Interface Data" workflow
   - Check the status (✅ Success, ❌ Failed, 🟡 Running)

2. **Manual Trigger**:
   ```
   1. Go to: https://github.com/avishekb9/WaveQTEX/actions/workflows/update-data.yml
   2. Click "Run workflow" button
   3. Select "main" branch
   4. Click "Run workflow"
   ```

3. **View Logs**:
   - Click on any workflow run
   - Expand each step to see detailed logs
   - Look for error messages or success confirmations

### Common GitHub Actions Issues

#### 1. **R Package Installation Fails**
**Symptoms**: Step "Install essential R packages" fails

**Solution**:
```yaml
# The workflow now uses only jsonlite which is stable
# If this fails, check system dependencies step
```

#### 2. **Data Generation Script Fails**
**Symptoms**: Step "Run simplified data pipeline" fails

**Check**:
- Look for error messages in R script output
- Verify `data-pipeline/generate-data-simple.R` exists
- Check file permissions

**Debug locally**:
```bash
cd WaveQTEX/data-pipeline
Rscript generate-data-simple.R
```

#### 3. **Git Push Fails**
**Symptoms**: Step "Push changes" fails with permission error

**Solution**:
- Check repository permissions
- Ensure `GITHUB_TOKEN` has write access
- The workflow includes retry mechanisms

#### 4. **No Data Changes Detected**
**Symptoms**: Workflow runs but no commit is made

**This is normal** - The workflow only commits when data actually changes.

### Data File Verification

#### Check Data Files Exist
```bash
# These files should exist in docs/data/:
- market_data.json (market prices and returns)
- network_data.json (network adjacency matrices)  
- agent_data.json (agent performance metrics)
- risk_data.json (risk components and indicators)
- metadata.json (update information)
- summary.json (generation summary)
```

#### Validate JSON Format
```bash
# Use online JSON validator or:
cat docs/data/metadata.json | python -m json.tool
```

#### Check File Sizes
```bash
ls -lh docs/data/
# Expected sizes:
# market_data.json: ~9KB
# network_data.json: ~140KB  
# agent_data.json: ~1KB
# risk_data.json: ~1KB
# metadata.json: ~600B
```

## Web Interface Issues

### Data Not Loading

#### 1. **Check Browser Console**
```javascript
// Open browser developer tools (F12)
// Look for errors in Console tab
// Common errors:
// - Failed to fetch data/market_data.json
// - JSON parse errors
// - Network connectivity issues
```

#### 2. **Verify Data Status Indicator**
- Look for status indicator in dashboard header
- **"Live Data"** (green dot) = Real data loaded successfully  
- **"Retrying"** (yellow dot) = Attempting to load data
- **"Fallback Data"** (red dot) = Using simulated data

#### 3. **Force Refresh Data**
```javascript
// Click the refresh button (🔄) in navigation
// Or manually trigger:
window.realDataHandler.forceRefresh();
```

#### 4. **Check GitHub Pages Status**
- GitHub Pages should deploy automatically after data updates
- Check: `https://github.com/avishekb9/WaveQTEX/deployments`

### Network Visualization Issues

#### 1. **3D Visualization Not Working**
**Check**:
- Browser supports WebGL
- Three.js library loaded correctly
- No JavaScript errors in console

**Test WebGL**:
```
Visit: https://get.webgl.org/
Should show spinning cube
```

#### 2. **Data Not Updating in Charts**
**Debug**:
```javascript
// Check if data handler is loaded
console.log(window.realDataHandler);

// Check latest data
console.log(window.realDataHandler.getData('market_data'));

// Force data update
document.dispatchEvent(new CustomEvent('refreshData'));
```

### Performance Issues

#### 1. **Slow Loading**
- Check internet connection
- Try refreshing the page
- Clear browser cache

#### 2. **High Memory Usage**
- Close unused browser tabs
- Refresh the page periodically
- Use latest Chrome/Firefox browser

## Monitoring and Maintenance

### Automated Monitoring

#### 1. **GitHub Actions Notifications**
```yaml
# Workflow sends email notifications on failure
# Check your GitHub notification settings
```

#### 2. **Data Freshness Check**
```javascript
// Check when data was last updated
const metadata = window.realDataHandler.getData('metadata');
console.log('Last updated:', metadata.last_updated);

// Check data age
const lastUpdate = new Date(metadata.last_updated);
const ageHours = (new Date() - lastUpdate) / (1000 * 60 * 60);
console.log('Data age (hours):', ageHours);
```

### Manual Maintenance

#### 1. **Force Data Regeneration**
```bash
# Option 1: Trigger GitHub Actions manually
# Go to: https://github.com/avishekb9/WaveQTEX/actions/workflows/update-data.yml
# Click "Run workflow"

# Option 2: Run locally and commit
cd WaveQTEX/data-pipeline
Rscript generate-data-simple.R
git add docs/data/
git commit -m "Manual data update"
git push
```

#### 2. **Update Workflow Schedule**
```yaml
# Edit .github/workflows/update-data.yml
# Change cron schedule:
schedule:
  - cron: '0 */3 * * *'  # Every 3 hours instead of 6
```

## Debugging Tools

### Local Development Setup

#### 1. **Test Data Generation**
```bash
# Install R and required packages
sudo apt-get install r-base
R -e "install.packages('jsonlite')"

# Run data generation
cd WaveQTEX/data-pipeline  
Rscript generate-data-simple.R

# Check output
ls -la ../docs/data/
```

#### 2. **Test Web Interface Locally**
```bash
# Start local server
cd WaveQTEX/docs
python -m http.server 8000

# Open browser
open http://localhost:8000
```

#### 3. **Check JSON Validity**
```bash
# Install jq for JSON processing
sudo apt-get install jq

# Validate and format JSON
jq '.' docs/data/market_data.json > /dev/null && echo "Valid JSON" || echo "Invalid JSON"

# Check specific data
jq '.US_SP500.current_price' docs/data/market_data.json
```

### Advanced Debugging

#### 1. **GitHub Actions Debug Mode**
```yaml
# Add to workflow for verbose logging
env:
  ACTIONS_RUNNER_DEBUG: true
  ACTIONS_STEP_DEBUG: true
```

#### 2. **R Script Debug Mode**
```r
# Add to generate-data-simple.R
options(warn = 2)  # Turn warnings into errors
options(error = traceback)  # Print stack trace on error
```

## Contact and Support

### Getting Help

1. **GitHub Issues**: Report bugs at https://github.com/avishekb9/WaveQTEX/issues
2. **Check Documentation**: Review README.md and code comments
3. **Community Support**: Check GitHub Discussions

### Providing Debug Information

When reporting issues, include:

```bash
# 1. GitHub Actions workflow URL
# 2. Browser console errors (F12 → Console)
# 3. Data file status:
ls -la docs/data/

# 4. System information:
echo "Browser: $(uname -a)"
echo "URL: $(curl -s https://avishekb9.github.io/WaveQTEX/)"

# 5. Last update time:
cat docs/data/metadata.json | grep last_updated
```

---

## Quick Fixes Checklist

- [ ] GitHub Actions workflow is enabled and has permissions
- [ ] Data files exist in `docs/data/` directory  
- [ ] JSON files are valid (no syntax errors)
- [ ] GitHub Pages is enabled for the repository
- [ ] Web interface loads without JavaScript errors
- [ ] Data status indicator shows current state
- [ ] Browser supports WebGL for 3D visualization
- [ ] No firewall blocking GitHub Pages access

**Most issues resolve automatically within 6 hours when the next scheduled workflow runs.**