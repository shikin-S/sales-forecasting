(function initializeReports() {
  const apiUrl = "http://localhost:8000/dashboard";
  let modelChart;
  let actualPredictedChart;
  document.addEventListener("DOMContentLoaded", loadEvaluation);

  function formatNumber(value) { return value === null || value === undefined || Number.isNaN(Number(value)) ? "--" : new Intl.NumberFormat("en-IN", {maximumFractionDigits: 2}).format(Number(value)); }
  function formatCurrency(value) { return value === "--" ? value : `₹${formatNumber(value)}`; }
  function formatPercent(value) { return value === "--" ? value : `${formatNumber(value)}%`; }
  function createCell(row, value) { const cell = document.createElement("td"); cell.textContent = value; row.appendChild(cell); }

  function displayEvaluationTable(models) {
    const body = document.getElementById("evaluation-table-body"); body.replaceChildren();
    models.forEach(model => { const row = document.createElement("tr"); createCell(row, model.model); createCell(row, model.status === "success" ? "✓ Success" : model.status); createCell(row, model.status === "success" ? formatCurrency(model.mae) : "--"); createCell(row, model.status === "success" ? formatCurrency(model.rmse) : "--"); createCell(row, model.status === "success" ? formatPercent(model.mape) : "--"); createCell(row, model.message || "Available"); body.appendChild(row); });
  }

  function displayBestModel(best) {
    document.getElementById("best-model-name").textContent = best?.name || "--";
    document.getElementById("best-model-metric").textContent = best?.metric || "--";
    document.getElementById("best-model-reason").textContent = best?.reason || "No valid model selected.";
  }
  function displayInsights(insights) {
    const list = document.getElementById("report-insights");
    if (!list) return;
    list.replaceChildren();
    Object.values(insights || {}).forEach(text => { const item = document.createElement("li"); item.textContent = text; list.appendChild(item); });
  }

  function chartOptions() { return {responsive: true, maintainAspectRatio: false, scales: {x: {grid: {display: false}}, y: {beginAtZero: true}}, plugins: {legend: {position: "bottom"}}}; }
  function createModelComparisonChart(models) {
    const valid = models.filter(model => model.status === "success"); const canvas = document.getElementById("model-error-chart");
    document.getElementById("model-error-empty").hidden = valid.length > 0; canvas.hidden = valid.length === 0; if (!valid.length || typeof Chart === "undefined") return;
    if (modelChart) modelChart.destroy(); modelChart = new Chart(canvas, {type: "bar", data: {labels: valid.map(model => model.model), datasets: [{label: "MAE", data: valid.map(model => model.mae), backgroundColor: "#174a5b"}, {label: "RMSE", data: valid.map(model => model.rmse), backgroundColor: "#d97745"}]}, options: chartOptions()});
  }
  function createActualVsPredictedChart(models) {
    const source = models.find(model => model.status === "success" && model.actual_vs_predicted?.length); const canvas = document.getElementById("actual-predicted-chart");
    document.getElementById("actual-predicted-empty").hidden = Boolean(source); canvas.hidden = !source; if (!source || typeof Chart === "undefined") return;
    if (actualPredictedChart) actualPredictedChart.destroy(); const rows = source.actual_vs_predicted; actualPredictedChart = new Chart(canvas, {type: "line", data: {labels: rows.map(row => row.date), datasets: [{label: "Actual Sales", data: rows.map(row => row.actual), borderColor: "#174a5b", tension: 0.2}, {label: `${source.model} Predicted`, data: rows.map(row => row.predicted), borderColor: "#d97745", borderDash: [6, 4], tension: 0.2}]}, options: chartOptions()});
  }
  function displayEvaluationError(message) { document.getElementById("evaluation-error").textContent = message; document.getElementById("evaluation-status").innerHTML = '<span class="status-dot"></span>Evaluation unavailable'; }

  async function loadEvaluation() {
    try {
      const response = await fetch(apiUrl); const data = await response.json();
      if (!response.ok || !data.success) throw new Error(data.message || "Unable to connect to the evaluation service.");
      const evaluation = data.evaluation; displayEvaluationTable(evaluation.models); displayBestModel(evaluation.best_model); displayInsights(data.insights); document.getElementById("evaluation-split").textContent = evaluation.available ? `${evaluation.evaluation.training_size} train · ${evaluation.evaluation.testing_size} test` : evaluation.message; createModelComparisonChart(evaluation.models); createActualVsPredictedChart(evaluation.models); document.getElementById("evaluation-status").innerHTML = evaluation.available ? '<span class="status-dot status-ready"></span>Report ready' : '<span class="status-dot"></span>Forecast evaluation unavailable'; window.dashboardReportData = data;
      document.querySelectorAll(".report-button").forEach(button => button.addEventListener("click", () => button.textContent.includes("Download") ? downloadReport(data) : document.getElementById("report-status").textContent = "Report data is displayed above."));
    } catch (error) { console.error("Evaluation request failed:", error); displayEvaluationError(error.message.includes("No processed") ? "No processed dataset available. Please upload and process a sales dataset first." : "Unable to connect to the evaluation service."); }
  }
  function downloadReport(data) {
    const analysis = data.analysis; const summary = analysis.summary || {}; const bestProduct = analysis.product_analysis?.top_products?.[0]?.product || "Not available"; const bestCategory = analysis.category_analysis?.category_sales?.[0]?.category || "Not available"; const evaluation = data.evaluation; const bestModel = evaluation.best_model;
    const insights = Object.values(data.insights || {}).map(text => `<li>${text}</li>`).join("");
    const html = `<html><head><meta charset="UTF-8"><title>Sales Forecasting and Analysis</title></head><body><h1>SALES FORECASTING AND ANALYSIS</h1><h2>Dataset Summary</h2><p>Dataset: ${data.dataset.file}</p><p>Analysis Period: ${analysis.dataset.date_range}</p><h2>Historical Sales Performance</h2><p>Total Sales: ${formatCurrency(summary.total_sales)}</p><p>Average Sales: ${formatCurrency(summary.average_sales)}</p><p>Total Quantity: ${formatNumber(summary.total_quantity)}</p><h2>Product and Category Performance</h2><p>Best Product: ${bestProduct}</p><p>Best Category: ${bestCategory}</p><h2>Model Evaluation</h2><p>Best Model: ${bestModel.name}</p><p>MAE: ${formatCurrency(evaluation.models.find(model => model.model === bestModel.name)?.mae)}</p><p>RMSE: ${formatCurrency(evaluation.models.find(model => model.model === bestModel.name)?.rmse)}</p><p>MAPE: ${formatPercent(evaluation.models.find(model => model.model === bestModel.name)?.mape)}</p><h2>Business Insights</h2><ul>${insights}</ul></body></html>`;
    const link = document.createElement("a"); link.href = URL.createObjectURL(new Blob([html], {type: "text/html"})); link.download = "sales-forecasting-report.html"; link.click(); URL.revokeObjectURL(link.href); document.getElementById("report-status").textContent = "HTML report downloaded.";
  }
})();
