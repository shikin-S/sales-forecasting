(function initializeForecast() {
  const apiUrl = "http://localhost:8000/forecast";
  let forecastChart;
  document.addEventListener("DOMContentLoaded", function onForecastLoaded() {
    const model = document.getElementById("forecast-model");
    const period = document.getElementById("forecast-period");
    const button = document.getElementById("forecast-button");
    const status = document.getElementById("forecast-status");
    const description = document.getElementById("model-description");
    const descriptions = {moving_average: "Uses recent historical sales values to estimate future sales.", linear_regression: "Models the relationship between sales and time to estimate future values.", exponential_smoothing: "Places greater weight on recent observations when generating forecasts.", holt_winters: "Models level, trend and seasonality when sufficient historical data is available.", arima: "Uses historical time-series patterns and differencing/autoregressive relationships to forecast future values.", automatic: "Compares available statistical models and selects the lowest validation-error result."};
    model.addEventListener("change", function updateDescription() { description.textContent = descriptions[model.value] || "Select a model to see a short explanation."; });
    button.addEventListener("click", generateForecast);

    function validateForecastInput() {
      if (!model.value) { status.textContent = "Please select a forecasting model."; return false; }
      if (!period.value) { status.textContent = "Please select a forecast period."; return false; }
      return true;
    }
    function formatNumber(value) { return value === null || value === undefined || Number.isNaN(Number(value)) ? "--" : new Intl.NumberFormat("en-IN", {maximumFractionDigits: 2}).format(Number(value)); }
    function formatCurrency(value) { return value === "--" ? value : `₹${formatNumber(value)}`; }
    function displayForecastSummary(data) {
      const values = data.forecast.map(row => Number(row.predicted_sales));
      document.getElementById("result-model").textContent = data.model;
      document.getElementById("result-frequency").textContent = data.frequency;
      document.getElementById("result-horizon").textContent = `${data.horizon} observations`;
      document.getElementById("result-total").textContent = formatCurrency(values.reduce((sum, value) => sum + value, 0));
      document.getElementById("result-average").textContent = formatCurrency(values.reduce((sum, value) => sum + value, 0) / values.length);
      document.getElementById("result-highest").textContent = formatCurrency(Math.max(...values));
      document.getElementById("result-lowest").textContent = formatCurrency(Math.min(...values));
      document.getElementById("forecast-generated-at").textContent = `Generated ${new Date().toLocaleString()}`;
    }
    function displayForecastTable(rows) {
      const body = document.getElementById("forecast-table-body"); body.replaceChildren();
      rows.forEach(row => { const tr = document.createElement("tr"); [row.date, formatCurrency(row.predicted_sales), row.lower == null || Number.isNaN(Number(row.lower)) ? "--" : formatCurrency(row.lower), row.upper == null || Number.isNaN(Number(row.upper)) ? "--" : formatCurrency(row.upper)].forEach(value => { const cell = document.createElement("td"); cell.textContent = value; tr.appendChild(cell); }); body.appendChild(tr); });
    }
    function createForecastChart(data) {
      const canvas = document.getElementById("forecast-chart"); if (forecastChart) forecastChart.destroy();
      const historicalLabels = data.historical.map(row => row.date); const forecastLabels = data.forecast.map(row => row.date); const labels = historicalLabels.concat(forecastLabels);
      const historicalValues = data.historical.map(row => row.sales).concat(forecastLabels.map(() => null)); const forecastValues = historicalLabels.map(() => null).concat(data.forecast.map(row => row.predicted_sales));
      forecastChart = new Chart(canvas, {type: "line", data: {labels, datasets: [{label: "Historical Sales", data: historicalValues, borderColor: "#174a5b", backgroundColor: "rgba(23, 74, 91, 0.1)", spanGaps: false, tension: 0.25}, {label: "Forecasted Sales", data: forecastValues, borderColor: "#d97745", backgroundColor: "rgba(217, 119, 69, 0.1)", spanGaps: false, borderDash: [6, 4], tension: 0.25}]}, options: {responsive: true, maintainAspectRatio: false, scales: {y: {beginAtZero: true}, x: {grid: {display: false}}}}});
    }
    function displayForecastError(message) { document.getElementById("forecast-results").hidden = true; document.getElementById("forecast-error").textContent = message; document.getElementById("forecast-status-badge").innerHTML = '<span class="status-dot"></span>Forecast unavailable'; }
    async function generateForecast() {
      if (!validateForecastInput()) return;
      button.disabled = true; document.getElementById("forecast-error").textContent = ""; document.getElementById("forecast-results").hidden = true; status.textContent = "Preparing historical data...";
      try {
        status.textContent = "Training forecasting model...";
        const response = await fetch(apiUrl, {method: "POST", headers: {"Content-Type": "application/json"}, body: JSON.stringify({model: model.value, period: Number(period.value)})});
        status.textContent = "Generating future predictions...";
        const data = await response.json();
        if (!response.ok || !data.success) { displayForecastError(data.message || "Unable to generate forecast."); return; }
        displayForecastSummary(data); displayForecastTable(data.forecast); createForecastChart(data); document.getElementById("forecast-results").hidden = false; status.textContent = "Forecast generated successfully."; document.getElementById("forecast-status-badge").innerHTML = '<span class="status-dot status-ready"></span>Forecast ready';
      } catch (error) { console.error("Forecast request failed:", error); displayForecastError("Unable to connect to the forecasting service."); } finally { button.disabled = false; }
    }
  });
})();
